import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.recruiterflow.schemas import (
    RecruiterFlowProfile,
    RFJob,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

RF_BASE_URL = "https://recruiterflow.com/api/external"


class AuthParameters(Struct):
    rf_api_key: Annotated[str, Meta(description="RecruiterFlow API key.")]


class ReadProfilesParameters(Struct):
    limit: Annotated[int, Meta(description="Number of profiles to return.")] = 100


class WriteProfilesParameters(Struct):
    pass


class ReadJobsParameters(Struct):
    limit: Annotated[int, Meta(description="Number of jobs to return.")] = 100

    only_open: Annotated[
        t.Optional[int], Meta(description="Only return open jobs. 1 -> True, 0-> False")
    ] = None


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    headers = {
        "RF-Api-Key": auth_parameters.rf_api_key,
    }

    profiles_url = f"{RF_BASE_URL}/candidate/list"

    params = {
        "items_per_page": parameters.limit,
        "current_page": 1,
        "include_files": 1,
        "include_count": True,
    }

    profiles = []
    while True:
        response = requests.get(profiles_url, headers=headers, params=params)

        if response.status_code != 200:
            adapter.error(
                "Failed to retrieve profiles from RecruiterFlow: %s",
                response.text,
            )
            raise Exception("Failed to retrieve profiles from RecruiterFlow")

        data = response.json()["data"]

        if not data:
            break

        profiles.extend(data)

        if len(profiles) >= parameters.limit:
            break

        params["current_page"] += 1

    if len(profiles) > parameters.limit:
        profiles = profiles[: parameters.limit]

    for profile in profiles:
        resume_link = None
        if profile["files"]:
            if len(profile["files"]) > 1:
                resume_link = next(
                    file["link"] for file in profile["files"] if file["is_primary"]
                )
            else:
                resume_link = profile["files"][0]["link"]
        if resume_link:
            resume_response = requests.get(resume_link)
            resume = resume_response.content
        else:
            resume = None
        profile["resume"] = resume

        yield profile


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    headers = {
        "RF-Api-Key": auth_parameters.rf_api_key,
    }
    post_profiles_url = f"{RF_BASE_URL}/candidate/add"

    for profile in items:
        resume = profile.pop("resume", None)
        response = requests.post(post_profiles_url, headers=headers, json=profile)

        if response.status_code != 200:
            adapter.error(
                "Failed to write profile to RecruiterFlow: %s",
                response.text,
            )
            failed_profiles.append(profile)

        # Add the resume file to the profile
        if resume:
            files = {
                "id": (None, response.json()["data"]["id"]),
                "file_name": (None, resume["original_file_name"]),
                "url": (None, resume["public_url"]),
            }

            resume_response = requests.post(
                f"{RF_BASE_URL}/contact/file/add",
                headers=headers,
                files=files,
            )

            if resume_response.status_code != 200:
                adapter.error(
                    "Failed to write resume to RecruiterFlow: %s",
                    resume_response.text,
                )

    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    headers = {
        "RF-Api-Key": auth_parameters.rf_api_key,
    }
    post_profiles_url = f"{RF_BASE_URL}/candidate/update"

    for profile in items:
        response = requests.post(post_profiles_url, headers=headers, json=profile)

        if response.status_code != 200:
            adapter.error(
                "Failed to write profile to RecruiterFlow: %s",
                response.text,
            )
            failed_profiles.append(profile)

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    headers = {
        "RF-Api-Key": auth_parameters.rf_api_key,
    }

    jobs_url = f"{RF_BASE_URL}/job/list"

    params = {
        "items_per_page": parameters.limit,
        "current_page": 1,
        "include_description": True,
        "include_count": True,
    }

    jobs = []

    while True:
        response = requests.get(jobs_url, headers=headers, params=params)

        if response.status_code != 200:
            adapter.error(
                "Failed to retrieve jobs from RecruiterFlow: %s",
                response.text,
            )
            raise Exception("Failed to retrieve jobs from RecruiterFlow")

        data = response.json()["data"]

        if not data:
            break

        jobs.extend(data)

        if len(jobs) >= parameters.limit:
            break

        params["current_page"] += 1

    if len(jobs) > parameters.limit:
        jobs = jobs[: parameters.limit]

    # Get locations info
    locations_response = requests.get(
        f"{RF_BASE_URL}/location/list",
        headers={"RF-Api-Key": auth_parameters.rf_api_key},
    )
    if locations_response.status_code != 200:
        adapter.error(
            "Failed to retrieve locations from RecruiterFlow: %s",
            locations_response.text,
        )
        raise Exception("Failed to retrieve locations from RecruiterFlow")
    locations = locations_response.json()["data"]

    for job in jobs:
        job_location_name = job["locations"][0]
        job_location = next(
            location for location in locations if location["name"] == job_location_name
        )
        job["locations"] = [job_location]
        yield job


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=RecruiterFlowProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles, update=read_profiles, archive=read_profiles
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=WriteProfilesParameters,
        ),
        function=merge(create=write_profiles, update=update_profiles),
    ),
)

JobsAisle = Aisle(
    name=Entity.job,
    schema=RFJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read_jobs, update=read_jobs, archive=read_jobs),
    ),
)
