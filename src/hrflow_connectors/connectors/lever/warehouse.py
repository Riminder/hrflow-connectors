import time
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field
from typing_extensions import Literal

from hrflow_connectors.connectors.lever.schemas import LeverJob, LeverProfile
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

LEVER_AUTH_ENDPOINT = "https://{auth_domain}.auth0.com"
LEVER_REDIRECT_URI = "https://marketplace-partners.hrflow.ai/partner/lever/login"
LEVER_BASE_URL = "https://{client_domain}.lever.co"
LEVER_JOBS_ENDPOINT = "https://{client_domain}.lever.co/v1/postings"
LEVER_OPPORTUNITIES_ENDPOINT = "https://{client_domain}.lever.co/v1/opportunities"

GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description="Endpoint to get the list of all jobs",
    url=LEVER_JOBS_ENDPOINT,
)
GET_ALL_PROFILES_ENDPOINT = ActionEndpoints(
    name="Get all profiles",
    description="Endpoint to get the list of all profiles",
    url=LEVER_OPPORTUNITIES_ENDPOINT,
)
POST_PROFILE_ENDPOINT = ActionEndpoints(
    name="Post Profile",
    description="Endpoint to create a new profile",
    url=LEVER_OPPORTUNITIES_ENDPOINT,
)


class LeverParameters(ParametersModel):
    auth_domain: str = Field(
        ...,
        description=(
            "Auth domain for authenticating with Lever API, exemple: sandbox-lever"
        ),
        field_type=FieldType.Auth,
    )
    client_domain: str = Field(
        ...,
        description=(
            "Client domain for authenticating with Lever API, exemple: api.sandbox"
        ),
        field_type=FieldType.Auth,
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


class ReadParameters(LeverParameters):
    limit: int = Field(
        100,
        description="Number of jobs to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(ReadParameters):
    pass


class WriteProfileParameters(LeverParameters):
    perform_as: str = Field(
        ...,
        description="User ID on behalf of whom the create action should be performed",
        field_type=FieldType.QueryParam,
    )
    parse: bool = Field(
        False,
        description="If true, parse resume for autofilling",
        field_type=FieldType.QueryParam,
    )
    perform_as_posting_owner: bool = Field(
        False,
        description="If true, set Opportunity owner to posting owner",
        field_type=FieldType.QueryParam,
    )


class ReadProfilesParameters(ReadParameters):
    pass


def get_or_refresh_tokens(
    auth_domain,
    client_id,
    client_secret,
    flow: Literal["authorization_code", "refresh_token"],
    code: str,
):
    lever_auth_endpoint = LEVER_AUTH_ENDPOINT.format(auth_domain=auth_domain)
    url = "{}/oauth/token".format(lever_auth_endpoint)
    redirect_uri = LEVER_REDIRECT_URI
    if flow == "authorization_code":
        request_data = {
            "client_id": client_id,
            "client_secret": client_secret,
            "grant_type": flow,
            "code": code,
            "redirect_uri": redirect_uri,
        }
    else:
        request_data = {
            "client_id": client_id,
            "client_secret": client_secret,
            "grant_type": flow,
            "refresh_token": code,
        }

    response = requests.post(url, data=request_data)
    if response.status_code == 200:
        response_data = response.json()
        access_token = response_data.get("access_token")
        new_refresh_token = response_data.get("refresh_token")
        return access_token, new_refresh_token

    raise Exception(
        f"Failed to obtain token. Status code: {response.status_code},"
        f" Response: {response.text}"
    )


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.auth_domain,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    jobs_url = LEVER_JOBS_ENDPOINT.format(client_domain=parameters.client_domain)
    offset = None
    while True:
        headers = {"Authorization": "Bearer " + token}
        params = {
            "limit": parameters.limit,
        }
        if offset is not None:
            params["offset"] = offset
        response = requests.get(jobs_url, headers=headers, params=params)
        if response.status_code == 200:
            jobs = response.json()["data"]
            for job in jobs:
                yield job
            if response.json().get("hasNext", False):
                offset = response.json().get("next", None)
            else:
                break
        elif response.status_code == 429:
            adapter.warning("Rate limit exceeded. Retrying after 5 second.")
            time.sleep(5)

        elif response.status_code == 401:
            adapter.warning("Access token has expired. Refreshing token and retrying.")
            token, refresh_token = get_or_refresh_tokens(
                parameters.auth_domain,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token,
            )
        else:
            raise Exception(
                "Failed to retrieve jobs from Lever. Status code:"
                f" {response.status_code}, Response: {response.text}"
            )


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.auth_domain,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    offset = None
    profiles_url = LEVER_OPPORTUNITIES_ENDPOINT.format(
        client_domain=parameters.client_domain
    )
    while True:
        headers = {"Authorization": "Bearer " + token}
        params = {
            "limit": parameters.limit,
        }
        if offset is not None:
            params["offset"] = offset
        response = requests.get(profiles_url, headers=headers, params=params)
        if response.status_code == 200:
            opportunities = response.json()["data"]
            for opportunity in opportunities:
                opportunity_id = opportunity["id"]
                profile_response = requests.get(
                    f"{profiles_url}/{opportunity_id}/resumes",
                    headers=headers,
                )
                if profile_response.status_code == 200:
                    profile_data = profile_response.json()
                    opportunity["profile"] = profile_data["data"]
                    yield opportunity
                else:
                    raise Exception(
                        "Failed to retrieve profiles for opportunity"
                        f" {opportunity_id}. Status code:"
                        f" {profile_response.status_code}, Response:"
                        f" {profile_response.text}"
                    )

            if response.json().get("hasNext", False):
                offset = response.json().get("next", None)
            else:
                break

        elif response.status_code == 429:
            adapter.warning("Rate limit exceeded. Retrying after 1 minute.")
            time.sleep(5)
        elif response.status_code == 401:
            adapter.warning("Access token has expired. Refreshing token and retrying.")
            token, refresh_token = get_or_refresh_tokens(
                parameters.auth_domain,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token,
            )
        else:
            raise Exception(
                "Failed to retrieve profiles from Lever. Status code:"
                f" {response.status_code}, Response: {response.text}"
            )


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.auth_domain,
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    url_post_opportunity = LEVER_OPPORTUNITIES_ENDPOINT.format(
        client_domain=parameters.client_domain
    )
    failed_profiles = []
    params = {
        "perform_as": parameters.perform_as,
        "parse": parameters.parse,
        "perform_as_posting_owner": parameters.perform_as_posting_owner,
    }
    i = 0
    while i < len(profiles):
        profile = profiles[i]
        profile.pop("file", None)
        headers = {"Authorization": "Bearer " + token}
        response = requests.post(
            url_post_opportunity,
            headers=headers,
            params=params,
            json=profile,
        )
        if response.status_code // 100 == 2:
            adapter.info("Successfully posted profile")
            i += 1
        elif response.status_code == 429:
            adapter.warning("Rate limit exceeded. Retrying after 1 minute.")
            time.sleep(5)
        elif response.status_code == 401:
            adapter.warning("Access token has expired. Refreshing token and retrying.")
            token, refresh_token = get_or_refresh_tokens(
                parameters.auth_domain,
                parameters.client_id,
                parameters.client_secret,
                "refresh_token",
                refresh_token,
            )
        else:
            failed_profiles.append(profile)
            adapter.error(
                "Failed to post profile. Status code: %s, Response: %s",
                response.status_code,
                response.text,
            )

    return failed_profiles


LeverJobWarehouse = Warehouse(
    name="Lever Jobs",
    data_schema=LeverJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[GET_ALL_JOBS_ENDPOINT],
    ),
)

LeverProfileWarehouse = Warehouse(
    name="Lever Profiles",
    data_schema=LeverProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
        endpoints=[GET_ALL_PROFILES_ENDPOINT],
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfileParameters,
        function=write,
        endpoints=[POST_PROFILE_ENDPOINT],
    ),
)
