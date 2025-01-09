import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.workable.schemas import (
    WorkableCandidate,
    WorkableJob,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

WORKABLE_BASE_URL = "https://{subdomain}.workable.com/spi/v3"


class AuthParameters(Struct):
    api_access_token: Annotated[
        str,
        Meta(
            description=(
                "The API access token for the Workable account, which can be generated"
                " from the Integrations section in the Workable backend settings."
            )
        ),
    ]
    subdomain: Annotated[
        str,
        Meta(
            description=(
                "The subdomain of the Workable account, can be retrieved with a GET"
                " request to the /account endpoint"
            ),
        ),
    ]


class JobState(str, Enum):
    draft = "draft"
    published = "published"
    archived = "archived"
    closed = "closed"


class JobsReadParameters(Struct, omit_defaults=True):
    state: Annotated[
        t.Optional[JobState],
        Meta(
            description=(
                "Returns jobs with the current state. Possible values (draft,"
                " published, archived & closed)."
            ),
        ),
    ] = None
    since_id: Annotated[
        t.Optional[str],
        Meta(
            description="Returns jobs with ID greater than the specified value.",
        ),
    ] = None
    max_id: Annotated[
        t.Optional[str],
        Meta(
            description="Returns jobs with ID less than the specified value.",
        ),
    ] = None
    created_after: Annotated[
        t.Optional[t.Union[str, int]],
        Meta(
            description="Returns jobs created after the specified timestamp/date time.",
        ),
    ] = None
    updated_after: Annotated[
        t.Optional[t.Union[str, int]],
        Meta(
            description="Returns jobs updated after the specified timestamp/date time.",
        ),
    ] = None


class ReadProfileParameters(Struct, omit_defaults=True):
    email: Annotated[
        t.Optional[str],
        Meta(
            description="The email of the candidate to filter by",
        ),
    ] = None
    shortcode: Annotated[
        t.Optional[str],
        Meta(
            description="The job's system generated code",
        ),
    ] = None
    stage: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The job's stage slug, can be retrieved from the /stages endpoint"
            ),
        ),
    ] = None
    max_id: Annotated[
        t.Optional[str],
        Meta(
            description="Returns candidates with ID less than the specified value.",
        ),
    ] = None
    created_after: Annotated[
        t.Optional[t.Union[str, int]],
        Meta(
            description=(
                "Returns candidates created after the specified timestamp/date time."
            ),
        ),
    ] = None
    updated_after: Annotated[
        t.Optional[t.Union[str, int]],
        Meta(
            description=(
                "Returns candidates updated after the specified timestamp/date time."
            ),
        ),
    ] = None


class WriteProfileParameters(Struct, omit_defaults=True):
    job_shortcode: Annotated[
        t.Optional[str],
        Meta(
            description="The job's shortcode to which the candidate is applying",
        ),
    ] = None
    stage: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The job's stage slug, can be retrieved from the /stages endpoint"
            ),
        ),
    ] = None
    add_to_talent_pool: Annotated[
        t.Optional[bool],
        Meta(
            description="Whether to add the candidate to the talent pool",
        ),
    ] = None


class UpdateProfileParameters(Struct, omit_defaults=True):
    pass


# TODO: add pagination
def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: JobsReadParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterator[t.Dict]:
    jobs_url = WORKABLE_BASE_URL.format(subdomain=auth_parameters.subdomain) + "/jobs"

    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    # params["include_fields"] = "shortcode"

    headers = {
        "Authorization": f"Bearer {auth_parameters.api_access_token}",
    }

    jobs = []
    url = jobs_url

    while True:
        response = requests.get(url, headers=headers, params=params)

        if not response.ok:
            adapter.error(f"Failed to read Workable jobs reason : {response.text}")
            raise Exception("Failed to read Workable jobs")

        jobs.extend(response.json().get("jobs", []))
        next_url = response.json().get("paging", {}).get("next")
        if not next_url:
            break
        url = next_url

    for job in jobs:
        full_job_response = requests.get(
            f"{jobs_url}/{job['shortcode']}",
            headers=headers,
        )
        if not full_job_response.ok:
            adapter.error(
                f"Failed to read Workable job reason : {full_job_response.text}"
            )

        yield full_job_response.json()


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfileParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterator[t.Dict]:
    profiles_url = (
        WORKABLE_BASE_URL.format(subdomain=auth_parameters.subdomain) + "/candidates"
    )

    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    headers = {
        "Authorization": f"Bearer {auth_parameters.api_access_token}",
    }

    profiles = []
    url = profiles_url

    while True:
        response = requests.get(url, headers=headers, params=params)

        if not response.ok:
            adapter.error(f"Failed to read Workable profiles reason : {response.text}")
            raise Exception("Failed to read Workable profiles")

        profiles.extend(response.json().get("candidates", []))
        next_url = response.json().get("paging", {}).get("next")
        if not next_url:
            break
        url = next_url

    for profile in profiles:
        full_profile_response = requests.get(
            f"{profiles_url}/{profile['id']}",
            headers=headers,
        )
        if not full_profile_response.ok:
            adapter.error(
                f"Failed to read Workable profile reason : {full_profile_response.text}"
            )

        full_profile = full_profile_response.json()["candidate"]

        resume_url = full_profile.get("resume_url")
        if resume_url:
            resume_response = requests.get(resume_url)
            if resume_response.ok:
                full_profile["resume"] = resume_response.content

        yield full_profile


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    params = {}
    if parameters.stage:
        params = {"stage": parameters.stage}

    for profile in items:
        if parameters.job_shortcode:
            post_candidate_url = (
                WORKABLE_BASE_URL.format(subdomain=auth_parameters.subdomain)
                + f"/jobs/{parameters.job_shortcode}/candidates"
            )
        if parameters.add_to_talent_pool:
            post_candidate_url = (
                WORKABLE_BASE_URL.format(subdomain=auth_parameters.subdomain)
                + "/talent_pool/candidates"
            )

        response = requests.post(
            post_candidate_url,
            headers={
                "Authorization": f"Bearer {auth_parameters.api_access_token}",
            },
            params=params,
            json=profile,
        )
        if not response.ok:
            adapter.error(f"Failed to write profile reason : {response.text}")
            failed_profiles.append(profile)
    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile in items:
        prodile_id = profile.pop("id")
        url = (
            WORKABLE_BASE_URL.format(subdomain=auth_parameters.subdomain)
            + f"/candidates/{prodile_id}"
        )

        response = requests.patch(
            url,
            headers={
                "Authorization": f"Bearer {auth_parameters.api_access_token}",
            },
            json=profile,
        )

        if not response.ok:
            adapter.error(f"Failed to update profile reason : {response.text}")
            failed_profiles.append(profile)

    return failed_profiles


JobsAisle = Aisle(
    name=Entity.job,
    schema=WorkableJob,
    read=ReadOperation(
        criterias=Criterias(
            create=JobsReadParameters,
            update=JobsReadParameters,
            archive=JobsReadParameters,
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
    schema=WorkableCandidate,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfileParameters,
            update=ReadProfileParameters,
            archive=ReadProfileParameters,
        ),
        function=merge(
            create=read_profiles,
            update=read_profiles,
            archive=read_profiles,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfileParameters,
            update=UpdateProfileParameters,
        ),
        function=merge(
            create=write_profiles,
            update=update_profiles,
        ),
    ),
)
