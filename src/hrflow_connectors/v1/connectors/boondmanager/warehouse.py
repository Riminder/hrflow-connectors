import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)
from hrflow_connectors.v1.connectors.boondmanager.schemas import (
    BoondManagerCandidate,
    BoondManagerOpportunity,
)
from hrflow_connectors.v1.connectors.boondmanager.utils.api import (
    BOONDMANAGER_BASE_URL,
    PAGE_SIZE,
    REQUEST_TIMEOUT,
    fetch_app_dictionary,
)
from hrflow_connectors.v1.connectors.boondmanager.utils.incremental import (
    item_to_read_from,
    parse_cursor,
    should_skip_item,
    to_api_date,
)
from hrflow_connectors.v1.connectors.boondmanager.utils.jwt import auth_headers


class BaseParameters(ParametersModel):
    client_key: str = Field(
        ...,
        description=(
            "BoondManager client key used to sign the JWT."
            " Found in Administration → API settings."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    client_token: str = Field(
        ...,
        description="BoondManager client token.",
        repr=False,
        field_type=FieldType.Auth,
    )
    user_token: str = Field(
        ...,
        description="BoondManager user token for the account making API calls.",
        repr=False,
        field_type=FieldType.Auth,
    )
    language: str = Field(
        default="fr",
        description=(
            "Language code used when resolving IDs to human-readable labels via the"
            " application dictionary (e.g. 'fr', 'en')."
        ),
        field_type=FieldType.Other,
    )


class ReadOpportunitiesParameters(BaseParameters):
    opportunity_states: t.Optional[str] = Field(
        default=None,
        description=(
            "Comma-separated list of opportunity state IDs to filter on."
            " Leave empty to retrieve all states."
            " Example: '0,1,2'."
        ),
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        default=None,
        description=(
            "Maximum number of opportunities to pull. Leave empty to pull all."
            " Useful for testing or incremental runs."
        ),
        field_type=FieldType.QueryParam,
    )


class ReadCandidatesParameters(BaseParameters):
    candidate_states: t.Optional[str] = Field(
        default=None,
        description=(
            "Comma-separated list of candidate state IDs to include."
            " Leave empty to retrieve all states."
            " BoondManager state IDs: 0=À traiter, 1=En cours de process,"
            " 2=Vivier, 3=Converti en Ressource, 4=Si projet,"
            " 5=Ne plus contacter, 6=Proposition en cours, 7=À recontacter plus tard,"
            " 9=Top profil."
            " Example: '0,1,2,3,4,6,9'."
        ),
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        default=None,
        description=(
            "Maximum number of candidates to pull. Leave empty to pull all."
            " Useful for testing or incremental runs."
        ),
        field_type=FieldType.QueryParam,
    )


class ReadCandidatesParsingParameters(BaseParameters):
    candidate_states: t.Optional[str] = Field(
        default=None,
        description=(
            "Comma-separated list of candidate state IDs to include."
            " Leave empty to retrieve all states."
            " BoondManager state IDs: 0=À traiter, 1=En cours de process,"
            " 2=Vivier, 3=Converti en Ressource, 4=Si projet,"
            " 5=Ne plus contacter, 6=Proposition en cours, 7=À recontacter plus tard,"
            " 9=Top profil."
            " Example: '0,1,2,3,4,6,9'."
        ),
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        default=None,
        description=(
            "Maximum number of candidates to pull. Leave empty to pull all."
            " Useful for testing or incremental runs."
        ),
        field_type=FieldType.QueryParam,
    )


def read_opportunities(
    adapter: LoggerAdapter,
    parameters: ReadOpportunitiesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    app_dictionary = fetch_app_dictionary(
        parameters.user_token,
        parameters.client_token,
        parameters.client_key,
        parameters.language,
    )

    last_update_date: t.Optional[str] = None
    last_id: t.Optional[str] = None

    list_params: dict = {"maxResults": PAGE_SIZE, "page": 1}
    if read_mode is ReadMode.incremental:
        # Always sort by updateDate in incremental mode so the cursor is meaningful
        # on every subsequent run — including the very first (no cursor yet).
        list_params["sort"] = "updateDate"
        if read_from:
            last_update_date, last_id = parse_cursor(read_from)
            list_params["period"] = "updated"
            if last_update_date:
                list_params["startDate"] = to_api_date(last_update_date)
    else:
        list_params["sort"] = "creationDate"
    if parameters.opportunity_states is not None:
        list_params["opportunityStates"] = parameters.opportunity_states

    page = 1
    total_expected = None
    collected = 0

    while True:
        list_params["page"] = page
        response = requests.get(
            url=f"{BOONDMANAGER_BASE_URL}/opportunities",
            headers=auth_headers(
                parameters.user_token, parameters.client_token, parameters.client_key
            ),
            params=list_params,
            timeout=REQUEST_TIMEOUT,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to list opportunities from BoondManager"
                " page={} status_code={} response={}".format(
                    page, response.status_code, response.text
                )
            )
            break

        payload = response.json()
        if total_expected is None:
            total_expected = payload.get("meta", {}).get("totals", {}).get("rows", 0)

        data = payload.get("data", [])
        if not data:
            break

        for item in data:
            if should_skip_item(item, last_update_date, last_id):
                continue

            opp_id = item["id"]
            detail_response = requests.get(
                url=f"{BOONDMANAGER_BASE_URL}/opportunities/{opp_id}/information",
                headers=auth_headers(
                    parameters.user_token,
                    parameters.client_token,
                    parameters.client_key,
                ),
                timeout=REQUEST_TIMEOUT,
            )
            if detail_response.status_code != 200:
                adapter.error(
                    "Failed to fetch opportunity details from BoondManager"
                    " opportunity_id={} status_code={} response={}".format(
                        opp_id, detail_response.status_code, detail_response.text
                    )
                )
                continue

            full_opp = detail_response.json().get("data", {})
            full_opp.setdefault("attributes", {}).update(item.get("attributes", {}))
            full_opp["_app_dictionary"] = app_dictionary
            collected += 1
            yield full_opp

            if parameters.limit is not None and collected >= parameters.limit:
                return

        if collected >= total_expected:
            break
        page += 1


def read_candidates(
    adapter: LoggerAdapter,
    parameters: ReadCandidatesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    app_dictionary = fetch_app_dictionary(
        parameters.user_token,
        parameters.client_token,
        parameters.client_key,
        parameters.language,
    )

    last_update_date: t.Optional[str] = None
    last_id: t.Optional[str] = None

    list_params: dict = {"maxResults": PAGE_SIZE, "page": 1}
    if read_mode is ReadMode.incremental:
        list_params["sort"] = "updateDate"
        if read_from:
            last_update_date, last_id = parse_cursor(read_from)
            list_params["period"] = "updated"
            if last_update_date:
                list_params["startDate"] = to_api_date(last_update_date)
    else:
        list_params["sort"] = "creationDate"
    if parameters.candidate_states is not None:
        list_params["candidateStates"] = parameters.candidate_states

    page = 1
    total_expected = None
    collected = 0

    while True:
        list_params["page"] = page
        response = requests.get(
            url=f"{BOONDMANAGER_BASE_URL}/candidates",
            headers=auth_headers(
                parameters.user_token, parameters.client_token, parameters.client_key
            ),
            params=list_params,
            timeout=REQUEST_TIMEOUT,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to list candidates from BoondManager"
                " page={} status_code={} response={}".format(
                    page, response.status_code, response.text
                )
            )
            break

        payload = response.json()
        if total_expected is None:
            total_expected = payload.get("meta", {}).get("totals", {}).get("rows", 0)

        data = payload.get("data", [])
        if not data:
            break

        for item in data:
            if should_skip_item(item, last_update_date, last_id):
                continue

            candidate_id = item["id"]
            detail_response = requests.get(
                url=f"{BOONDMANAGER_BASE_URL}/candidates/{candidate_id}/information",
                headers=auth_headers(
                    parameters.user_token,
                    parameters.client_token,
                    parameters.client_key,
                ),
                timeout=REQUEST_TIMEOUT,
            )
            if detail_response.status_code != 200:
                adapter.error(
                    "Failed to fetch candidate details from BoondManager"
                    " candidate_id={} status_code={} response={}".format(
                        candidate_id,
                        detail_response.status_code,
                        detail_response.text,
                    )
                )
                continue

            full_candidate = detail_response.json().get("data", {})
            full_candidate.setdefault("attributes", {}).update(
                item.get("attributes", {})
            )
            full_candidate["_app_dictionary"] = app_dictionary

            collected += 1
            yield full_candidate

            if parameters.limit is not None and collected >= parameters.limit:
                return

        if collected >= total_expected:
            break
        page += 1


def read_candidates_parsing(
    adapter: LoggerAdapter,
    parameters: ReadCandidatesParsingParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    list_params: dict = {"sort": "creationDate", "maxResults": PAGE_SIZE, "page": 1}
    if parameters.candidate_states is not None:
        list_params["candidateStates"] = parameters.candidate_states

    page = 1
    total_expected = None
    collected = 0

    while True:
        list_params["page"] = page
        response = requests.get(
            url=f"{BOONDMANAGER_BASE_URL}/candidates",
            headers=auth_headers(
                parameters.user_token, parameters.client_token, parameters.client_key
            ),
            params=list_params,
            timeout=REQUEST_TIMEOUT,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to list candidates from BoondManager"
                " page={} status_code={} response={}".format(
                    page, response.status_code, response.text
                )
            )
            if collected == 0:
                raise Exception(
                    "Failed to list candidates from BoondManager on first page:"
                    " status_code={}".format(response.status_code)
                )
            break

        payload = response.json()
        if total_expected is None:
            total_expected = payload.get("meta", {}).get("totals", {}).get("rows", 0)

        data = payload.get("data", [])
        if not data:
            break

        for item in data:
            candidate_id = item["id"]

            info_response = requests.get(
                url=f"{BOONDMANAGER_BASE_URL}/candidates/{candidate_id}/information",
                headers=auth_headers(
                    parameters.user_token,
                    parameters.client_token,
                    parameters.client_key,
                ),
                timeout=REQUEST_TIMEOUT,
            )
            if info_response.status_code != 200:
                adapter.warning(
                    "Failed to fetch candidate information for candidate_id={}"
                    " status_code={}".format(candidate_id, info_response.status_code)
                )
                continue

            info_data = info_response.json().get("data", {})
            resume_entries = (
                info_data.get("relationships", {}).get("resumes", {}).get("data", [])
            )
            if not resume_entries:
                continue

            resume_id = resume_entries[-1].get("id")
            resume_response = requests.get(
                url=f"{BOONDMANAGER_BASE_URL}/documents/{resume_id}",
                headers=auth_headers(
                    parameters.user_token,
                    parameters.client_token,
                    parameters.client_key,
                ),
                timeout=REQUEST_TIMEOUT,
            )
            if resume_response.status_code != 200:
                adapter.warning(
                    "Failed to fetch resume for candidate_id={}"
                    " resume_id={} status_code={}".format(
                        candidate_id, resume_id, resume_response.status_code
                    )
                )
                continue

            candidate = {
                "id": candidate_id,
                "attributes": info_data.get("attributes", {}),
                "_resume_bytes": resume_response.content,
            }

            collected += 1
            yield candidate

            if parameters.limit is not None and collected >= parameters.limit:
                return

        if collected >= total_expected:
            break
        page += 1


BoondManagerOpportunityWarehouse = Warehouse(
    name="BoondManager Opportunities",
    data_schema=BoondManagerOpportunity,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadOpportunitiesParameters,
        function=read_opportunities,
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
)

BoondManagerCandidateWarehouse = Warehouse(
    name="BoondManager Candidates",
    data_schema=BoondManagerCandidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadCandidatesParameters,
        function=read_candidates,
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
)

BoondManagerCandidateParsingWarehouse = Warehouse(
    name="BoondManager Candidates",
    data_schema=BoondManagerCandidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadCandidatesParsingParameters,
        function=read_candidates_parsing,
    ),
)
