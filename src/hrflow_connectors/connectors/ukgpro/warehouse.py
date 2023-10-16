import json
import typing as t
from logging import LoggerAdapter
from os.path import join
from time import time

import requests
from pydantic import Field

from hrflow_connectors.connectors.ukgpro.schemas import (
    UKGProAttachDocument,
    UKGProAuthentication,
    UKGProDomain,
    UKGProFileType,
)
from hrflow_connectors.connectors.ukgpro.utils.auth import (
    _auth_headers_and_token_limit_get,
)
from hrflow_connectors.connectors.ukgpro.utils.enums import UKGProDocumentType
from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

_UKGPRO_BASE_FURL = "https://{environment}/{tenant}/"
_UKGPRO_OPPORTUNITIES_ENDPOINT = "api/opportunities"
_UKGPRO_CANDIDATES_ENDPOINT = "api/candidates"
_UKGPRO_CANDIDATES_LOOKUP_ENDPOINT = "api/candidates-lookup"
_UKGPRO_CANDIDATES_DOCUMENTS_FENDPOINT = "api/candidates/{candidate_id}/documents"
_UKGPRO_OPPORTUNITIES_FURL = join(_UKGPRO_BASE_FURL, _UKGPRO_OPPORTUNITIES_ENDPOINT)
_UKGPRO_CANDIDATES_FURL = join(_UKGPRO_BASE_FURL, _UKGPRO_CANDIDATES_ENDPOINT)
_UKGPRO_CANDIDATES_DOCUMENTS_FURL = join(
    _UKGPRO_BASE_FURL, _UKGPRO_CANDIDATES_DOCUMENTS_FENDPOINT
)
_UKGPRO_CANDIDATES_LOOKUP_FURL = join(
    _UKGPRO_BASE_FURL, _UKGPRO_CANDIDATES_LOOKUP_ENDPOINT
)


class UKGProReadJobsParameters(ParametersModel):
    auth_data: UKGProAuthentication = Field(..., field_type=FieldType.Auth)
    domain: UKGProDomain = Field(..., field_type=FieldType.Other)


class UKGProWriteProfileParameters(ParametersModel):
    auth_data: UKGProAuthentication = Field(..., field_type=FieldType.Auth)
    domain: UKGProDomain = Field(..., field_type=FieldType.Other)


def read_jobs(
    adapter: LoggerAdapter,
    parameters: UKGProReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    """
    Reads `UKGPro.Candidate`s and yields them

    Returns:
      Iterable of dictionaries representing the respective object
    """

    params = dict(page=1, per_page=500)
    token_limit = 0  # init value
    url = _UKGPRO_OPPORTUNITIES_FURL.format(
        environment=parameters.domain.environment,
        tenant=parameters.domain.tenant,
    )

    while True:
        if time() >= token_limit:
            headers, token_limit = _auth_headers_and_token_limit_get(
                parameters.auth_data
            )

        response = requests.get(url, headers=headers, params=params)

        if response.status_code != requests.codes.ok:
            adapter.error(
                "Failed to read opportunities from UKGPro with"
                f" status_code={response.status_code}, url={url}, params={params} and"
                f" response={response.text}"
            )
            raise RuntimeError("failed to read opportunities from UKGPro")
        else:
            data = response.json()["data"]
            for opportunity in data:
                yield opportunity
            if len(data) < params["per_page"]:
                break

        params["page"] += 1


def _email_already_used(
    domain: UKGProDomain, email: t.Optional[str], headers: t.Dict
) -> bool:
    """
    Returns:
      True whether the email is already used, False otherwise.
    """

    if not email:
        return False

    response = requests.post(
        _UKGPRO_CANDIDATES_LOOKUP_FURL.format(
            environment=domain.environment, tenant=domain.tenant
        ),
        headers=headers,
        json=dict(email=email),
    )

    return (
        response.status_code == requests.codes.ok
        or response.status_code == requests.codes.conflict
    )


def _files_object_get(document: UKGProAttachDocument, file_type: str) -> t.Dict:
    """
    Returns:
      Request file object
    """

    metadata = dict(file_name=document.file_name, document_type=document.document_type)
    return dict(
        metadata=(None, json.dumps(metadata), file_type),
        file=(document.file_name, document.file_data, file_type),
    )


def _profile_enrich_with_attachments(
    profile: t.Dict, domain: UKGProDomain, candidate_id: str
) -> None:
    """
    Enriches (in place) `UKGPro.Candidate` with `HrFlow.Attachment` from
    `HrFlow.Profile`

    Returns:
      None
    """

    succeeded = 0
    MAX_SUCCESSFULLY_ATTACHED = 10
    url = _UKGPRO_CANDIDATES_DOCUMENTS_FURL.format(
        environment=domain.environment, tenant=domain.tenant, candidate_id=candidate_id
    )

    attachments = profile.get("attachments")

    if not isinstance(attachments, list):
        return

    for attachment in attachments:
        try:
            attachment_type = attachment["type"].lower()
            if not (attachment_type in ["resume", "cover"]):
                continue
            document_type = UKGProDocumentType[attachment_type.upper()]
            public_url = attachment["public_url"]
            file_name = public_url.split("/")[-1]
            file_extu = file_name.split(".")[-1].upper()
            file_type = UKGProFileType[file_extu]
        except Exception:
            continue
        response = requests.get(public_url)
        if response.status_code != requests.codes.ok:
            continue
        try:
            document = UKGProAttachDocument(
                document_type=document_type,
                file_data=response.content,
                file_name=file_name,
            )
        except Exception:
            continue
        response = requests.post(url, files=_files_object_get(document, file_type))
        succeeded += response.status_code == requests.codes.created
        if succeeded >= MAX_SUCCESSFULLY_ATTACHED:
            break


def write_profile(
    adapter: LoggerAdapter,
    parameters: UKGProWriteProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    """
    Writes given `HrFlow.Profile`s to UKGPro

    Returns:
      List of dictionaries representing the profiles for which the action has failed
    """

    failed_profiles = []
    token_limit = 0
    curl = _UKGPRO_CANDIDATES_FURL.format(
        environment=parameters.domain.environment, tenant=parameters.domain.tenant
    )

    for profile in list(profiles):
        if time() <= token_limit:
            headers, token_limit = _auth_headers_and_token_limit_get(
                parameters.auth_data
            )

        if _email_already_used(parameters.domain, profile["info"]["email"], headers):
            failed_profiles.append(profile)
            continue

        response = requests.post(curl, headers=headers, json=profile)

        if response.status_code != requests.codes.created:
            failed_profiles.append(profile)
            adapter.warning(
                "failed to index profile to UKGPro with"
                f" status_code={response.status_code}, body={profile} and"
                f" response={response.text}"
            )
        else:  # enrich with attachments
            _profile_enrich_with_attachments(
                profile, parameters.domain, response.json()["id"]
            )

    return failed_profiles


UKGProProfilesWarehouse = Warehouse(
    name="UKGPro Profiles",
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=UKGProWriteProfileParameters,
        function=write_profile,
        endpoint=[
            ActionEndpoints(
                name="Post Candidates",
                description=(
                    "Send a POST candidates request to create a new candidate record in"
                    " UKG Pro Recruiting. Each candidate record requires a unique email"
                    " address. If the email address you submit for the new candidate is"
                    " not found on any existing external candidate record, the new"
                    " candidate record will be created, and a unique identifier will be"
                    " included in the return payload. Retain that unique identifier for"
                    " submitting applications or documents for the candidate."
                ),
                url=_UKGPRO_CANDIDATES_FURL,
            )
        ],
    ),
)


UKGProJobsWarehouse = Warehouse(
    name="UKGPro Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=UKGProReadJobsParameters,
        function=read_jobs,
        endpoint=[
            ActionEndpoints(
                name="Get Opportunities",
                description=(
                    "Use the GET Opportunities endpoint to retrieve job postings from"
                    " UKG Pro Recruiting, and find the posting that will be used to add"
                    " applications via the Candidate Import API."
                ),
                url=_UKGPRO_OPPORTUNITIES_FURL,
            )
        ],
    ),
)
