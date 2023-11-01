import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.jobadder.schemas import (
    CandidatesAdditionalParams,
    JobadderCandidate,
    JobadderJob,
    JobsAdditionalParams,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

JOBS_ENDPOINT = "https://api.jobadder.com/v2/jobs"
CANDIDATE_ENDPOINT = "https://api.jobadder.com/v2/candidates"


class BaseReadingParameters(ParametersModel):
    authorization_url: str = Field(
        ...,
        description="Authorization URL for obtaining authorization code",
        field_type=FieldType.Auth,
    )
    client_id: str = Field(
        ...,
        description="Client ID for authenticating with Jobadder API",
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client secret for authenticating with Jobadder API",
        field_type=FieldType.Auth,
    )
    authorization_code: str = Field(
        ...,
        description="Authorization code for obtaining access token",
        field_type=FieldType.Auth,
    )
    redirect_uri: str = Field(
        ...,
        description="Redirect URI for obtaining access token",
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(JobsAdditionalParams, BaseReadingParameters):
    limit: int = Field(
        100,
        description="Number of items to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        0,
        description="Number of items to skip",
        field_type=FieldType.QueryParam,
    )


class ReadCandidatesParameters(CandidatesAdditionalParams, BaseReadingParameters):
    limit: int = Field(
        100,
        description="Number of items to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        0,
        description="Number of items to skip",
        field_type=FieldType.QueryParam,
    )


class WriteCandidateParameters(BaseReadingParameters):
    XAllowDuplicates: bool = Field(
        False,
        description="Allow duplicates",
        field_type=FieldType.QueryParam,
    )


def get_or_refresh_tokens(
    authorization_url,
    redirect_uri,
    client_id,
    client_secret,
    grant_type,
    authorization_code=None,
    refresh_token=None,
):
    if authorization_code:
        request_data = {
            "client_id": client_id,
            "client_secret": client_secret,
            "grant_type": grant_type,
            "code": authorization_code,
            "redirect_uri": redirect_uri,
        }
    elif refresh_token:
        request_data = {
            "client_id": client_id,
            "client_secret": client_secret,
            "grant_type": grant_type,
            "refresh_token": refresh_token,
        }
    else:
        return None, None

    response = requests.post(authorization_url, data=request_data)
    if response.status_code == 200:
        response_data = response.json()
        access_token = response_data.get("access_token")
        new_refresh_token = response_data.get("refresh_token")
        return access_token, new_refresh_token
    else:
        return None, None


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.authorization_url,
        parameters.redirect_uri,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        authorization_code=parameters.authorization_code,
    )
    adapter.info("Fetching jobs from Jobadder")
    jobs_id = []
    offset = 0
    params = parameters
    params.pop("authorization_url")
    params.pop("client_id")
    params.pop("client_secret")
    params.pop("authorization_code")
    params.pop("redirect_uri")

    headers = {"Authorization": f"Bearer {token}"}
    while True:
        params["offset"] = offset
        response = requests.get(JOBS_ENDPOINT, params=params, headers=headers)
        if response.status_code == 200:
            response_data = response.json()
            jobs = response_data.get("items")
            if jobs:
                for job in jobs:
                    jobs_id.append(job["jobId"])
            else:
                break
            if not jobs["links"]["next"]:
                break
            offset = jobs["links"]["next"]
        elif response.status_code == 401 or response.status_code == 403:
            token, refresh_token = get_or_refresh_tokens(
                parameters.authorization_url,
                parameters.redirect_uri,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if token:
                headers = {"Authorization": f"Bearer {token}"}
            else:
                adapter.error("Token refresh failed.")
                break
        else:
            adapter.error(f"Failed to fetch jobs from Jobadder: {response.text}")
            break
    for job_id in jobs_id:
        response = requests.get(f"{JOBS_ENDPOINT}/{job_id}", headers=headers)
        if response.status_code == 200:
            yield response.json()
        else:
            adapter.error(
                f"Failed to fetch job {job_id} from Jobadder: {response.text}"
            )


def read_candidates(
    adapter: LoggerAdapter,
    parameters: ReadCandidatesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.authorization_url,
        parameters.redirect_uri,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        authorization_code=parameters.authorization_code,
    )
    adapter.info("Fetching candidates from Jobadder")
    candidates_id = []
    offset = 0
    params = parameters
    params.pop("authorization_url")
    params.pop("client_id")
    params.pop("client_secret")
    params.pop("authorization_code")
    params.pop("redirect_uri")
    headers = {"Authorization": f"Bearer {token}"}
    while True:
        params["offset"] = offset
        response = requests.get(CANDIDATE_ENDPOINT, params=params, headers=headers)
        if response.status_code == 200:
            response_data = response.json()
            candidates = response_data.get("items")
            if candidates:
                for candidate in candidates:
                    candidates_id.append(candidate["candidateId"])
            else:
                break
            if not candidates["links"]["next"]:
                break
            offset = candidates["links"]["next"]
        elif response.status_code == 401 or response.status_code == 403:
            token, refresh_token = get_or_refresh_tokens(
                parameters.authorization_url,
                parameters.redirect_uri,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if token:
                headers = {"Authorization": f"Bearer {token}"}
            else:
                adapter.error("Token refresh failed.")
                break
        else:
            adapter.error(f"Failed to fetch candidates from Jobadder: {response.text}")
            break
    for candidate_id in candidates_id:
        response = requests.get(f"{CANDIDATE_ENDPOINT}/{candidate_id}", headers=headers)
        if response.status_code == 200:
            yield response.json()
        else:
            adapter.error(
                f"Failed to fetch candidate {candidate_id} from Jobadder:"
                f" {response.text}"
            )


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteCandidateParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    token, refresh_token = get_or_refresh_tokens(
        parameters.authorization_url,
        parameters.redirect_uri,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        authorization_code=parameters.authorization_code,
    )
    headers = {
        "Authorization": f"Bearer {token}",
        "X-Allow-Duplicates": parameters.XAllowDuplicates,
    }
    for profile in profiles:
        response = requests.post(CANDIDATE_ENDPOINT, json=profile, headers=headers)
        if response.status_code == 200:
            adapter.info(
                f"Pushed profile(reference={profile['reference']}, id={profile['id']})"
                " to Jobadder"
            )
        elif response.status_code == 401 or response.status_code == 403:
            token, refresh_token = get_or_refresh_tokens(
                parameters.authorization_url,
                parameters.redirect_uri,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if token:
                headers = {"Authorization": f"Bearer {token}"}
                response = requests.post(
                    CANDIDATE_ENDPOINT, json=profile, headers=headers
                )
                if response.status_code == 200:
                    adapter.info(
                        f"Pushed profile(reference={profile['reference']},"
                        f" id={profile['id']}) to Jobadder"
                    )
                else:
                    adapter.error(
                        f"Failed to push profile(reference={profile['reference']},"
                        f" id={profile['id']}) to Jobadder with error={response.text}"
                    )
                    failed.append(profile)
            else:
                adapter.error("Token refresh failed.")
                failed.append(profile)
        else:
            adapter.error(
                f"Failed to push profile(reference={profile['reference']},"
                f" id={profile['id']}) to Jobadder with error={response.text}"
            )
            failed.append(profile)
    return failed


JobadderProfileWarehouse = Warehouse(
    name="Jobadder Profiles",
    data_schema=JobadderCandidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadCandidatesParameters,
        function=read_candidates,
    ),
    write=WarehouseWriteAction(
        parameters=WriteCandidateParameters,
        function=write_profiles,
    ),
)

JobadderJobWarehouse = Warehouse(
    name="Jobadder Jobs",
    data_schema=JobadderJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)
