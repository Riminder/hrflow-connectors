import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.zoho.schemas import Candidate, JobOpenings
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

AUTHORIZATION_URL = "{accounts_url}/oauth/v2/auth"
JOBS_URL = "{accounts_url}/recruit/private/json/JobOpenings/getRecords"
CANDIDATES_URL = "{accounts_url}/recruit/private/json/Candidates/getRecords"


class BaseZohoParameters(ParametersModel):
    accounts_url: str = Field(
        ...,
        description="URL for fetching accounts",
        field_type=FieldType.QueryParam,
    )
    client_id: str = Field(
        ...,
        description="Client ID for authenticating with Lever API",
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client secret for authenticating with Lever API",
        field_type=FieldType.Auth,
    )
    authorization_code: str = Field(
        ...,
        description="Authorization code for obtaining access token",
        field_type=FieldType.Auth,
    )


class ReadingParameters(ParametersModel):
    selectColumns: str = Field(
        "All",
        description="Columns to fetch",
        field_type=FieldType.QueryParam,
    )
    fromIndex: int = Field(
        1,
        description="Index of the first job to fetch",
        field_type=FieldType.QueryParam,
    )
    toIndex: int = Field(
        200,
        description="Index of the last job to fetch",
        field_type=FieldType.QueryParam,
    )
    sortColumnString: str = Field(
        "Modified Time",
        description="Column to sort by",
        field_type=FieldType.QueryParam,
    )
    sortOrderString: str = Field(
        "desc",
        description="Sort order",
        field_type=FieldType.QueryParam,
    )
    lastModifiedTime: str = Field(
        None,
        description="Fetch jobs modified after this time",
        field_type=FieldType.QueryParam,
    )
    newFormat: int = Field(
        1,
        description="whiche format to use for response",
        field_type=FieldType.QueryParam,
    )
    version: int = Field(
        2,
        description="API version",
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(BaseZohoParameters, ReadingParameters):
    publishURL: bool = Field(
        False,
        description="Whether to include publishURL in the response",
        field_type=FieldType.QueryParam,
    )


class ReadCandidatesParameters(BaseZohoParameters, ReadingParameters):
    pass


class WriteCandidatesParameters(BaseZohoParameters):
    candidateOwner: str = Field(
        "HrFlow",
        description="Owner of the candidate",
        field_type=FieldType.QueryParam,
    )
    pass


def get_or_refresh_tokens(
    accounts_url,
    client_id,
    client_secret,
    grant_type,
    authorization_code=None,
    refresh_token=None,
):
    url = AUTHORIZATION_URL.format(accounts_url=accounts_url)
    redirect_uri = "https://marketplace-partners.hrflow.ai/partner/zoho/login"
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
        raise Exception("Grant type not supported")

    response = requests.post(url, data=request_data)
    if response.status_code == 200:
        response_data = response.json()
        access_token = response_data.get("access_token")
        new_refresh_token = response_data.get("refresh_token")
        return access_token, new_refresh_token
    else:
        raise Exception(
            f"Failed to get access token: {response.text}, response status code"
            f" {response.status_code}"
        )


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    jobs_url = JOBS_URL.format(accounts_url=parameters.accounts_url)
    access_token, refresh_token = get_or_refresh_tokens(
        parameters.accounts_url,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    if not access_token:
        raise Exception("Failed to get access token")
    headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
    params = {
        "newFormat": parameters.newFormat,
        "version": parameters.version,
        "selectColumns": parameters.selectColumns,
        "fromIndex": parameters.fromIndex,
        "toIndex": parameters.toIndex,
        "sortColumnString": parameters.sortColumnString,
        "sortOrderString": parameters.sortOrderString,
        "lastModifiedTime": parameters.lastModifiedTime,
    }
    while True:
        response = requests.get(jobs_url, headers=headers, params=params)
        if response.status_code == 200:
            response_data = response.json()
            jobs = response_data.get("response").get("result").get("JobOpenings")
            for job in jobs:
                yield job
            if not jobs:
                break
            params["fromIndex"] = params["toIndex"] + 1
            params["toIndex"] = params["fromIndex"] + len(jobs)
        elif response.status_code == 4834:
            access_token, refresh_token = get_or_refresh_tokens(
                parameters.accounts_url,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if not access_token:
                raise Exception("Failed to refresh access token")
            headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        else:
            adapter.error(
                f"Failed to fetch jobs: {response.text}, response status code"
                f" {response.status_code}"
            )


def read_candidates(
    adapter: LoggerAdapter,
    parameters: ReadCandidatesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    candidates_url = CANDIDATES_URL.format(accounts_url=parameters.accounts_url)
    access_token, refresh_token = get_or_refresh_tokens(
        parameters.accounts_url,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )

    headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
    params = {
        "newFormat": parameters.newFormat,
        "version": parameters.version,
        "selectColumns": parameters.selectColumns,
        "fromIndex": parameters.fromIndex,
        "toIndex": parameters.toIndex,
        "sortColumnString": parameters.sortColumnString,
        "sortOrderString": parameters.sortOrderString,
        "lastModifiedTime": parameters.lastModifiedTime,
    }
    while True:
        response = requests.get(candidates_url, headers=headers, params=params)
        if response.status_code == 200:
            response_data = response.json()
            candidates = response_data.get("response").get("result").get("Candidates")
            for candidate in candidates:
                yield candidate
            if not candidates:
                break
            params["fromIndex"] = params["toIndex"] + 1
            params["toIndex"] = params["fromIndex"] + len(candidates)
        elif response.status_code == 4834:
            access_token, refresh_token = get_or_refresh_tokens(
                parameters.accounts_url,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if not access_token:
                raise Exception("Failed to refresh access token")
            headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        else:
            adapter.error(
                f"Failed to fetch candidates: {response.text}, response status code"
                f" {response.status_code}"
            )
            break


def write_candidates(
    adapter: LoggerAdapter,
    parameters: WriteCandidatesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    candidates_url = CANDIDATES_URL.format(accounts_url=parameters.accounts_url)
    access_token, refresh_token = get_or_refresh_tokens(
        parameters.accounts_url,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    if not access_token:
        raise Exception("Failed to get access token")
    headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}

    for profile in profiles:
        profile["candidateOwner"] = parameters.candidateOwner
        response = requests.post(candidates_url, headers=headers, json=profile)
        if response.status_code == 200:
            adapter.info("Candidate {} created".format(profile["candidateID"]))
        elif response.status_code == 4834:
            access_token, refresh_token = get_or_refresh_tokens(
                parameters.accounts_url,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token=refresh_token,
            )
            if not access_token:
                raise Exception("Failed to refresh access token")
            headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
            response = requests.post(candidates_url, headers=headers, json=profile)
            if response.status_code == 200:
                adapter.info("Candidate {} created".format(profile["candidateID"]))
            else:
                adapter.error(
                    f"Failed to create candidate: {response.text}, response status code"
                    f" {response.status_code}"
                )
                failed_profiles.append(profile)
        else:
            adapter.error(
                f"Failed to create candidate: {response.text}, response status code"
                f" {response.status_code}"
            )
            failed_profiles.append(profile)
    return failed_profiles


ZohoJobWarehouse = Warehouse(
    name="Zoho Jobs",
    data_schema=JobOpenings,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)
ZohoCandidatesWarehouse = Warehouse(
    name="Zoho Candidates",
    data_schema=Candidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadCandidatesParameters,
        function=read_candidates,
    ),
    write=WarehouseWriteAction(
        parameters=WriteCandidatesParameters,
        function=write_candidates,
    ),
)
