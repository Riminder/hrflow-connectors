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
    REQUEST_TIMEOUT,
    fetch_app_dictionary,
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
    fetch_resume: bool = Field(
        default=False,
        description=(
            "When True, the connector will attempt to download the most recent"
            " resume file for each candidate and attach it as a base64-encoded"
            " resume URL. This significantly increases runtime due to one extra"
            " API call per candidate."
        ),
        field_type=FieldType.Other,
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

    list_params: dict = {"sort": "creationDate", "page": 1}
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

    list_params: dict = {"sort": "creationDate", "page": 1}
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
            candidate_id = item["id"]
            detail_response = requests.get(
                url=f"{BOONDMANAGER_BASE_URL}/candidates/{candidate_id}",
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

            if parameters.fetch_resume:
                attrs = full_candidate.get("attributes", {})
                if attrs.get("numberOfResumes", 0) > 0:
                    resumes_data = (
                        full_candidate.get("relationships", {})
                        .get("resumes", {})
                        .get("data", [])
                    )
                    if resumes_data:
                        resume_id = resumes_data[-1].get("id")
                        resume_response = requests.get(
                            url=f"{BOONDMANAGER_BASE_URL}/documents/{resume_id}",
                            headers=auth_headers(
                                parameters.user_token,
                                parameters.client_token,
                                parameters.client_key,
                            ),
                            timeout=REQUEST_TIMEOUT,
                        )
                        if resume_response.status_code == 200:
                            full_candidate["_resume_bytes"] = resume_response.content
                        else:
                            adapter.warning(
                                "Failed to fetch resume for candidate_id={}"
                                " resume_id={} status_code={}".format(
                                    candidate_id,
                                    resume_id,
                                    resume_response.status_code,
                                )
                            )

            collected += 1
            yield full_candidate

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
    ),
)

BoondManagerCandidateWarehouse = Warehouse(
    name="BoondManager Candidates",
    data_schema=BoondManagerCandidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadCandidatesParameters,
        function=read_candidates,
    ),
)
