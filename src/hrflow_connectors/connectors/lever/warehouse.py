import typing as t
import time
from logging import LoggerAdapter

import requests
from pydantic import Field

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

LEVER_BASE_URL = "https://api.sandbox.lever.co"
LEVER_JOBS_ENDPOINT = "https://api.sandbox.lever.co/v1/postings"
LEVER_OPPORTUNITIES_ENDPOINT = "https://api.sandbox.lever.co/v1/opportunities"

GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description="Endpoint to get the list of all jobs",
    url="https://api.sandbox.lever.co/v1/postings",
)
GET_ALL_PROFILES_ENDPOINT = ActionEndpoints(
    name="Get all profiles",
    description="Endpoint to get the list of all profiles",
    url="https://api.sandbox.lever.co/v1/opportunities",
)
POST_PROFILE_ENDPOINT = ActionEndpoints(
    name="Post Profile",
    description="Endpoint to create a new profile",
    url="https://api.sandbox.lever.co/v1/opportunities",
)


class ReadJobsParameters(ParametersModel):
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
    limit: int = Field(
        100,
        description="Number of jobs to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )


class WriteProfileParameters(ParametersModel):
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
    limit: int = Field(
        100,
        description="Number of jobs to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )
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


class ReadProfilesParameters(ParametersModel):
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
    limit: int = Field(
        100,
        description="Number of jobs to fetch per request (max: 100)",
        field_type=FieldType.QueryParam,
    )


def get_or_refresh_tokens(
    client_id, client_secret, grant_type, authorization_code=None, refresh_token=None
):
    url = "https://sandbox-lever.auth0.com/oauth/token"
    redirect_uri = "https://marketplace-partners.hrflow.ai/partner/lever/login"
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

    response = requests.post(url, data=request_data)
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
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    adapter.info("Fetching jobs from Lever")
    offset = None
    while True:
        if token:
            headers = {"Authorization": "Bearer " + token}
            try:
                params = {
                    "limit": parameters.limit,
                }
                if offset is not None:
                    params["offset"] = offset
                response = requests.get(
                    LEVER_JOBS_ENDPOINT, headers=headers, params=params
                )
                if response.status_code == 200:
                    jobs = response.json()["data"]
                    for job in jobs:
                        yield job
                    # After fetching a page of results
                    if response.json().get("hasNext", False):
                        offset = response.json().get("next", None)
                    else:
                        break  # No more pages, exit the loop
                # manage rate limit
                elif response.status_code == 429:
                    adapter.error("Rate limit exceeded. Retrying after 1 minute.")
                    time.sleep(60)

                else:
                    adapter.error(
                        "Failed to retrieve jobs from Lever. Status code:"
                        f" {response.status_code}, Response: {response.text}"
                    )
                    break
            except requests.exceptions.RequestException as e:
                if "401" in str(e):
                    adapter.error(
                        "Access token has expired. Refreshing token and retrying."
                    )
                    token, refresh_token = get_or_refresh_tokens(
                        parameters.client_id,
                        parameters.client_secret,
                        "refresh_token",
                        refresh_token=refresh_token,
                    )
                    if not token:
                        adapter.error("Failed to refresh token.")
                        break
                else:
                    adapter.error(f"Request failed with error: {e}")
                    break
        else:
            adapter.error("Failed to obtain initial access token.")
            break


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    adapter.info("Fetching opportunities from Lever")
    offset = None
    while True:
        if token:
            headers = {"Authorization": "Bearer " + token}
            try:
                params = {
                    "limit": parameters.limit,
                }
                if offset is not None:
                    params["offset"] = offset
                response = requests.get(
                    LEVER_OPPORTUNITIES_ENDPOINT, headers=headers, params=params
                )
                if response.status_code == 200:
                    opportunities = response.json()["data"]
                    for opportunity in opportunities:
                        opportunity_id = opportunity["id"]
                        profile_response = requests.get(
                            f"{LEVER_OPPORTUNITIES_ENDPOINT}/{opportunity_id}/resumes",
                            headers=headers,
                        )
                        if profile_response.status_code == 200:
                            profile_data = profile_response.json()
                            opportunity["profile"] = profile_data["data"]
                            yield opportunity
                        else:
                            adapter.error(
                                "Failed to retrieve profiles for opportunity"
                                f" {opportunity_id}. Status code:"
                                f" {profile_response.status_code}, Response:"
                                f" {profile_response.text}"
                            )

                    # After fetching a page of results
                    if response.json().get("hasNext", False):
                        offset = response.json().get("next", None)
                    else:
                        break  # No more pages, exit the loop
                # manage rate limit
                elif response.status_code == 429:
                    adapter.error("Rate limit exceeded. Retrying after 1 minute.")
                    time.sleep(60)
                else:
                    adapter.error(
                        "Failed to retrieve opportunities from Lever. Status code:"
                        f" {response.status_code}, Response: {response.text}"
                    )
                    break
            except requests.exceptions.RequestException as e:
                if "401" in str(e):
                    adapter.error(
                        "Access token has expired. Refreshing token and retrying."
                    )
                    token, refresh_token = get_or_refresh_tokens(
                        parameters.client_id,
                        parameters.client_secret,
                        "refresh_token",
                        refresh_token=refresh_token,
                    )
                    if not token:
                        adapter.error("Failed to refresh token.")
                        break
                else:
                    adapter.error(f"Request failed with error: {e}")
                    break
        else:
            adapter.error("Failed to obtain initial access token.")
            break


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    token, refresh_token = get_or_refresh_tokens(
        parameters.client_id,
        parameters.client_secret,
        "authorization_code",
        parameters.authorization_code,
    )
    adapter.info(f"Pushing {len(profiles)} profiles to Lever")
    failed_profiles = []
    for profile in profiles:
        if token:
            while True:
                # attachment_url = None
                # if profile.get("file"):
                #     attachment_url = profile["file"].get("public_url")
                profile.pop("file", None)
                headers = {"Authorization": "Bearer " + token}
                params = {
                    "perform_as": parameters.perform_as,
                    "parse": parameters.parse,
                    "perform_as_posting_owner": parameters.perform_as_posting_owner,
                }
                response = requests.post(
                    LEVER_OPPORTUNITIES_ENDPOINT,
                    headers=headers,
                    params=params,
                    json=profile,
                )
                if response.status_code // 100 == 2:
                    adapter.info("Successfully posted profile")
                    # opportunity_id = response.json()["data"]["id"]
                    # if attachment_url:
                    #     cv_binary = download_cv_as_binary(attachment_url)
                    #     if cv_binary:
                    #         files = {
                    #             "resumeFile": (
                    #                 "resume.pdf",
                    #                 cv_binary,
                    #                 "application/ocet-stream",
                    #             )
                    #         }
                    #         headers = {
                    #             "Authorization": "Bearer " + token,
                    #             "Content-Type": "multipart/form-data",
                    #         }
                    #         response = requests.post(
                    #             LEVER_OPPORTUNITIES_ENDPOINT + "/" + opportunity_id,
                    #             headers=headers,
                    #             files=files,
                    #         )
                    #         if response.status_code // 100 == 2:
                    #             adapter.info("Successfully posted resume")
                    break
                elif response.status_code == 429:
                    adapter.error("Rate limit exceeded. Retrying after 1 minute.")
                    time.sleep(60)
                elif response.status_code == 401:
                    adapter.error(
                        "Access token has expired. Refreshing token and retrying."
                    )
                    token, refresh_token = get_or_refresh_tokens(
                        parameters.client_id,
                        parameters.client_secret,
                        "refresh_token",
                        refresh_token=refresh_token,
                    )
                    if not token:
                        adapter.error("Failed to refresh token.")
                        failed_profiles.append(profile)
                        return failed_profiles
                else:
                    adapter.error(
                        "Error posting opportunity. Status code:",
                        response.status_code,
                        "Response:",
                        response.text,
                    )
                    failed_profiles.append(profile)
                    break

    return failed_profiles


def download_cv_as_binary(attachment_url: str) -> bytes:
    # Download the CV from the attachment URL and return it as binary data
    try:
        response = requests.get(attachment_url)
        if response.status_code == 200:
            return response.content
        else:
            return None
    except Exception:
        return None


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
