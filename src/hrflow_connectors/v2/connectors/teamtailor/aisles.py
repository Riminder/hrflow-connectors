import enum
import pdb
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.teamtailor.schema import (
    TeamtailorCandidateAttributes,
    TeamtailorJob,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

JOBS_ENDPOINT = "https://api.teamtailor.com/v1/jobs"
CANDIDATES_ENDPOINT = "https://api.teamtailor.com/v1/candidates"


class status(str, enum.Enum):
    published = "published"
    unlisted = "unlisted"
    archived = "archived"
    draft = "draft"
    scheduled = "scheduled"
    all = "all"


class RemoteStatus(str, enum.Enum):
    none = "none"
    hybrid = "hybrid"
    temporary = "temporary"
    fully = "fully"


class JobFeed(str, enum.Enum):
    public = "public"
    internal = "internal"


class TeamTailorBoolean(str, enum.Enum):
    true = "true"
    false = "false"


class AuthParameters(Struct):
    api_key: Annotated[
        str,
        Meta(
            description=(
                "API key for authenticating with the Teamtailor API. You can generate"
                " it in the Teamtailor app under Settings > Integrations > API Keys."
            ),
        ),
    ]

    X_Api_Version: Annotated[
        str,
        Meta(
            description="API version for the Teamtailor API. Default is '20240404'.",
        ),
    ] = "20240404"


class WriteProfilesParameters(Struct):
    pass


class ReadJobsParameters(Struct):
    filter_status: Annotated[
        t.Optional[status],
        Meta(
            description="Filter by job status. Available statuses: {}".format(
                [e.value for e in status]
            ),
        ),
    ] = None

    filter_feed: Annotated[
        t.Optional[JobFeed],
        Meta(
            description="Status of a job. One of {}".format([e.value for e in JobFeed]),
        ),
    ] = None

    filter_department: Annotated[
        t.Optional[str],
        Meta(description="Filter by department id"),
    ] = None

    filter_role: Annotated[
        t.Optional[str],
        Meta(description="Filter by role id"),
    ] = None

    filter_locations: Annotated[
        t.Optional[str],
        Meta(description="Filter by location id"),
    ] = None

    filter_regions: Annotated[
        t.Optional[str],
        Meta(description="Filter by region id"),
    ] = None

    filter_tags: Annotated[
        t.Optional[str],
        Meta(description="Filter by tags"),
    ] = None

    filter_remote_status: Annotated[
        t.Optional[RemoteStatus],
        Meta(
            description=(
                "Filter by remote status. Available remote statuses: {}".format(
                    [e.value for e in RemoteStatus]
                )
            ),
        ),
    ] = None

    filter_created_at_from: Annotated[
        t.Optional[str],
        Meta(description="Filter by created-at older than this date."),
    ] = None

    filter_created_at_to: Annotated[
        t.Optional[str],
        Meta(description="Filter by created-at newer than this date."),
    ] = None

    filter_updated_at_from: Annotated[
        t.Optional[str],
        Meta(description="Filter by updated-at older than this date."),
    ] = None

    filter_updated_at_to: Annotated[
        t.Optional[str],
        Meta(description="Filter by updated-at newer than this date."),
    ] = None

    sort: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Default sorting based on pinned status and publish"
                " date.\n'-pinned,date'"
            )
        ),
    ] = None


class ReadProfilesParameters(Struct):
    filter_email: Annotated[
        t.Optional[str],
        Meta(description="Filter by email address"),
    ] = None

    filter_department: Annotated[
        t.Optional[str],
        Meta(description="Filter by department"),
    ] = None

    filter_role: Annotated[
        t.Optional[str],
        Meta(description="Filter by role"),
    ] = None

    filter_locations: Annotated[
        t.Optional[str],
        Meta(description="Filter by location"),
    ] = None

    filter_regions: Annotated[
        t.Optional[str],
        Meta(description="Filter by region"),
    ] = None

    filter_created_at_from: Annotated[
        t.Optional[str],
        Meta(description="Filter by created-at older than this date."),
    ] = None

    filter_created_at_to: Annotated[
        t.Optional[str],
        Meta(description="Filter by created-at newer than this date."),
    ] = None

    filter_updated_at_from: Annotated[
        t.Optional[str],
        Meta(description="Filter by updated-at older than this date."),
    ] = None

    filter_updated_at_to: Annotated[
        t.Optional[str],
        Meta(description="Filter by updated-at newer than this date."),
    ] = None

    filter_connected: Annotated[
        t.Optional[TeamTailorBoolean],
        Meta(description="Filter candidates who has connected."),
    ] = None

    sort: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Sort list by any of the candidate attributes. Use -id to sort by id"
                " descending."
            ),
        ),
    ] = None


def enrich_location(job_id: str, headers: t.Dict) -> t.Dict:
    """
    Get_location sends a request to get the job location from its API endpoint
    """
    response = requests.get(
        "{}/{}/location".format(JOBS_ENDPOINT, job_id),
        headers=headers,
    )

    if not response.ok:
        raise Exception("Failed to get job location from Teamtailor.")

    location = response.json().get("data")
    if location is not None:
        location_attribute = location.get("attributes")
        text = location_attribute["address"]
        city = location_attribute["city"]
        country = location_attribute["country"]
        lat = float(location_attribute["lat"])
        lng = float(location_attribute["long"])
        zip = location_attribute["zip"]
        geojson = dict(
            text=text,
            city=city,
            country=country,
            zip=zip,
        )
        location_obj = dict(text=text, geojson=geojson, lat=lat, lng=lng)
        return location_obj
    return dict(text="", geojson="", lat=None, lng=None)


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    headers = {
        "Authorization": f"Token token={auth_parameters.api_key}",
        "X-Api-Version": auth_parameters.X_Api_Version,
    }

    params = asdict(parameters)
    params_dict = {
        "filter[status]": params.get("filter_status"),
        "filter[feed]": params.get("filter_feed"),
        "filter[department]": params.get("filter_department"),
        "filter[role]": params.get("filter_role"),
        "filter[locations]": params.get("filter_locations"),
        "filter[regions]": params.get("filter_regions"),
        "filter[tags]": params.get("filter_tags"),
        "filter[remote-status]": params.get("filter_remote_status"),
        "filter[created-at][from]": params.get("filter_created_at_from"),
        "filter[created-at][to]": params.get("filter_created_at_to"),
        "filter[updated-at][from]": params.get("filter_updated_at_from"),
        "filter[updated-at][to]": params.get("filter_updated_at_to"),
        "sort": params.get("sort"),
    }

    params_dict = {k: v for k, v in params_dict.items() if v is not None}

    url = JOBS_ENDPOINT
    all_jobs = []
    while True:
        response = requests.get(url, headers=headers, params=params_dict)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Teamtailor status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Teamtailor")
        response = response.json()
        jobs = response.get("data")

        all_jobs += jobs
        if response.get("links").get("next") is None:
            break
        else:
            url = response.get("links").get("next")

    adapter.info("Pulling {} jobs from Teamtailor API".format(len(all_jobs)))

    for job in all_jobs:
        job_location = enrich_location(job.get("id"), headers)
        job["location"] = job_location
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    headers = {
        "Authorization": f"Token token={auth_parameters.api_key}",
        "X-Api-Version": auth_parameters.X_Api_Version,
    }

    params = asdict(parameters)
    params_dict = {
        "filter[email]": params.get("filter_email"),
        "filter[department]": params.get("filter_department"),
        "filter[role]": params.get("filter_role"),
        "filter[locations]": params.get("filter_locations"),
        "filter[regions]": params.get("filter_regions"),
        "filter[created-at][from]": params.get("filter_created_at_from"),
        "filter[created-at][to]": params.get("filter_created_at_to"),
        "filter[updated-at][from]": params.get("filter_updated_at_from"),
        "filter[updated-at][to]": params.get("filter_updated_at_to"),
        "filter[connected]": params.get("filter_connected"),
        "sort": params.get("sort"),
    }

    params_dict = {k: v for k, v in params_dict.items() if v is not None}

    url = CANDIDATES_ENDPOINT
    all_profiles = []
    while True:
        response = requests.get(url, headers=headers, params=params_dict)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Teamtailor status_code={} response={}"
                .format(response.status_code, response.text)
            )
            raise Exception("Failed to pull profiles from Teamtailor")
        response = response.json()
        profiles = response.get("data")

        all_profiles += profiles
        if response.get("links").get("next") is None:
            break
        else:
            url = response.get("links").get("next")

    adapter.info("Pulling {} profiles from Teamtailor API".format(len(all_profiles)))

    for profile in all_profiles:
        resume_url = profile["attributes"].get("resume")
        if resume_url is not None:
            resume_response = requests.get(resume_url)
            if resume_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to get resume from Teamtailor status_code={} response={}"
                    .format(resume_response.status_code, resume_response.text)
                )
                raise Exception("Failed to get resume from Teamtailor")
            profile["resume"] = dict(raw=resume_response.content)
        yield profile


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile in items:
        pdb.set_trace()
        profile_obj = {
            "data": {
                "type": "candidates",
                "attributes": profile,
            },
        }

        headers = {
            "Authorization": f"Token token={auth_parameters.api_key}",
            "X-Api-Version": auth_parameters.X_Api_Version,
            "Content-Type": "application/vnd.api+json",
        }

        response = requests.post(CANDIDATES_ENDPOINT, headers=headers, json=profile_obj)

        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Teamtailor,  status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile in items:
        profile_id = profile.pop("id")
        profile_obj = {
            "data": {
                "id": profile_id,
                "attributes": profile,
                "type": "candidates",
            },
        }

        headers = {
            "Authorization": f"Token token={auth_parameters.api_key}",
            "X-Api-Version": auth_parameters.X_Api_Version,
            "Content-Type": "application/vnd.api+json",
        }
        response = requests.patch(
            "{}/{}".format(CANDIDATES_ENDPOINT, profile_id),
            headers=headers,
            json=profile_obj,
        )

        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update profile to Teamtailor,  status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


JobsAisle = Aisle(
    name=Entity.job,
    schema=TeamtailorJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=TeamtailorCandidateAttributes,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles,
            update=read_profiles,
            archive=read_profiles,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=WriteProfilesParameters,
        ),
        function=merge(
            create=write_profiles,
            update=update_profiles,
        ),
    ),
)
