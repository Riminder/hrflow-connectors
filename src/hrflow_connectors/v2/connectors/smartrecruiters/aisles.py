import enum
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.smartrecruiters.schemas import (
    SmartRecruitersJob,
    SmartRecruitersProfile,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    ReadOperation,
    WriteOperation,
    merge,
)

SMARTRECRUITERS_JOBS_ENDPOINT = "https://api.smartrecruiters.com/jobs"
SMARTRECRUITERS_PROFILES_ENDPOINT = "https://api.smartrecruiters.com/candidates"
SMARTRECRUITERS_ENDPOINT_LIMIT = 100

GET_ALL_JOBS_ENDPOINT = Endpoint(
    name="Get all jobs",
    description=(
        "Endpoint to search jobs by traditional params (offset, limit...)"
        " and get the list of all jobs with their ids, the request method"
        " is `GET`"
    ),
    url="https://developers.smartrecruiters.com/reference/jobsall-1",
)
GET_JOB_ENDPOINT = Endpoint(
    name="Get job",
    description=(
        "Endpoint to get the content of a job with a given id, a jobId parameter is"
        " required, the request method is `GET`"
    ),
    url="https://developers.smartrecruiters.com/reference/jobsget-1",
)
UPDATE_JOB_ENDPOINT = Endpoint(
    name="Update job",
    description=(
        "Endpoint to update a job with a given id, a jobId parameter is required, the"
        " request method is `PUT`"
    ),
    url="https://developers.smartrecruiters.com/reference/jobspatch-1",
)
GET_CANDIDATE_ENDPOINT = Endpoint(
    name="Get candidate",
    description=(
        "Endpoint to get the content of a candidate with a given id, a candidateId"
        " parameter is required, the request method is `GET`"
    ),
    url="https://developers.smartrecruiters.com/reference/candidatesget-1",
)

POST_CANDIDATE_ENDPOINT = Endpoint(
    name="Post Candidate",
    description=(
        "Endpoint to create a new candidate and assign to a talent pool, the request"
        " method is `POST`"
    ),
    url="https://developers.smartrecruiters.com/reference/candidatesadd-1",
)

UPDATE_CANDIDATE_ENDPOINT = Endpoint(
    name="Update Candidate",
    description=(
        "Endpoint to update a candidate with a given id, a candidateId parameter is"
        " required, the request method is `PUT`"
    ),
    url="https://developers.smartrecruiters.com/reference/candidatespatch-1",
)

DELETE_CANDIDATE_ENDPOINT = Endpoint(
    name="Delete Candidate",
    description=(
        "Endpoint to delete a candidate with a given id, a candidateId parameter is"
        " required, the request method is `DELETE`"
    ),
    url="https://developers.smartrecruiters.com/reference/candidatesdelete-1",
)


class JobPostingStatus(str, enum.Enum):
    public = "PUBLIC"
    internal = "INTERNAL"
    not_published = "NOT_PUBLISHED"
    private = "PRIVATE"


class JobStatus(str, enum.Enum):
    created = "CREATED"
    sourcing = "SOURCING"
    filled = "FILLED"
    interview = "INTERVIEW"
    offer = "OFFER"
    cancelled = "CANCELLED"
    on_hold = "ON_HOLD"


class OnboardingStatus(str, enum.Enum):
    ready_to_onboard = "READY_TO_ONBOARD"
    onboarding_successful = "ONBOARDING_SUCCESSFUL"
    onboarding_failed = "ONBOARDING_FAILED"


class AuthParameters(Struct):
    x_smart_token: Annotated[
        str,
        Meta(
            description="X-SmartToken used to access SmartRecruiters API",
        ),
    ]


class ReadJobsParameters(Struct):
    query: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Case insensitive full-text query against job title e.g. java developer"
            ),
        ),
    ] = None
    updated_after: Annotated[
        t.Optional[str],
        Meta(
            description="ISO8601-formatted time boundaries for the job update time",
        ),
    ] = None
    posting_status: Annotated[
        t.Optional[JobPostingStatus],
        Meta(
            description="Posting status of a job. One of {}".format(
                [e.value for e in JobPostingStatus]
            ),
        ),
    ] = None
    job_status: Annotated[
        t.Optional[JobStatus],
        Meta(
            description="Status of a job. One of {}".format(
                [e.value for e in JobStatus]
            ),
        ),
    ] = None
    limit: Annotated[
        int,
        Meta(description="Number of items to pull from SmartRecruiters at a time."),
    ] = SMARTRECRUITERS_ENDPOINT_LIMIT


class ReadProfilesParameters(Struct):
    query: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "keyword search, for more infromation see SmartRecruiters HelpCenter"
            ),
        ),
    ] = None
    job_ids: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="List of job ids to filter candidates by",
        ),
    ] = None
    tags: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="List of tags to filter candidates by",
        ),
    ] = None
    onboarding_status: Annotated[
        t.Optional[OnboardingStatus],
        Meta(
            description="Onboarding status of a candidate",
        ),
    ] = None
    status: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="candidate’s status filter in a context of a job",
        ),
    ] = None
    limit: Annotated[
        int,
        Meta(
            description=(
                "Number of items to pull from SmartRecruiters at a time. Not matter"
                " what value is supplied it is capped at {}".format(
                    SMARTRECRUITERS_ENDPOINT_LIMIT
                )
            ),
        ),
    ] = SMARTRECRUITERS_ENDPOINT_LIMIT
    updated_after: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ISO8601-formatted time boundaries for the candidate update time"
            ),
        ),
    ] = None


class WriteProfilesParameters(Struct):
    pass


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    # The limit parameter in the endpoint specifies the number of items per page,
    # not the total number of items returned. Therefore, we need to manually limit
    # the number of items retrieved.

    page = None
    jobs = []
    while True:
        params = dict(
            q=parameters.query,
            updatedAfter=parameters.updated_after,
            postingStatus=parameters.posting_status,
            status=parameters.job_status,
            limit=(
                parameters.limit
                if parameters.limit < SMARTRECRUITERS_ENDPOINT_LIMIT
                else SMARTRECRUITERS_ENDPOINT_LIMIT
            ),
            pageId=page,
        )
        response = requests.get(
            SMARTRECRUITERS_JOBS_ENDPOINT,
            headers={"X-SmartToken": auth_parameters.x_smart_token},
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from SmartRecruiters params={}"
                " status_code={} response={}".format(
                    params, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from SmartRecruiters")
        response = response.json()
        jobs += response["content"]
        if len(jobs) == 0 or len(jobs) >= parameters.limit:
            break

        page = response["nextPageId"]

    if len(jobs) > parameters.limit:
        jobs = jobs[: parameters.limit]

    for job in jobs:
        full_job_response = requests.get(
            "{}/{}".format(SMARTRECRUITERS_JOBS_ENDPOINT, job["id"]),
            headers={"X-SmartToken": auth_parameters.x_smart_token},
        )
        if full_job_response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs details from SmartRecruiters job_id={}"
                " status_code={} response={}".format(
                    job["id"],
                    full_job_response.status_code,
                    full_job_response.text,
                )
            )
            raise Exception("Failed to pull jobs from SmartRecruiters")
        yield full_job_response.json()


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
):
    # The limit parameter in the endpoint specifies the number of items per page,
    # not the total number of items returned. Therefore, we need to manually limit
    # the number of items retrieved.

    page = None
    profiles = []
    while True:
        params = dict(
            q=parameters.query,
            updatedAfter=parameters.updated_after,
            jobId=parameters.job_ids,
            tag=parameters.tags,
            onboardingStatus=parameters.onboarding_status,
            status=parameters.status,
            limit=(
                parameters.limit
                if parameters.limit < SMARTRECRUITERS_ENDPOINT_LIMIT
                else SMARTRECRUITERS_ENDPOINT_LIMIT
            ),
            pageId=page,
        )
        response = requests.get(
            SMARTRECRUITERS_PROFILES_ENDPOINT,
            headers={"x-smarttoken": auth_parameters.x_smart_token},
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from SmartRecruiters params={}"
                " status_code={} response={}".format(
                    params, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from SmartRecruiters")
        response = response.json()
        profiles += response["content"]
        if len(profiles) == 0 or len(profiles) >= parameters.limit:
            break

        page = response["nextPageId"]

    if len(profiles) > parameters.limit:
        profiles = profiles[: parameters.limit]

    for profile in profiles:
        full_profile_response = requests.get(
            "{}/{}".format(SMARTRECRUITERS_PROFILES_ENDPOINT, profile["id"]),
            headers={"x-smarttoken": auth_parameters.x_smart_token},
        )
        if full_profile_response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profile details from SmartRecruiters profile_id={}"
                " status_code={} response={}".format(
                    profile["id"],
                    full_profile_response.status_code,
                    full_profile_response.text,
                )
            )
            raise Exception("Failed to pull profiles from SmartRecruiters")
        yield full_profile_response.json()


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile in items:
        response = requests.post(
            SMARTRECRUITERS_PROFILES_ENDPOINT,
            headers={"x-smarttoken": auth_parameters.x_smart_token},
            json=profile,
        )
        if response.status_code != 201:
            adapter.error(
                "Failed to push profile to SmartRecruiters"
                " status_code={} response={}".format(
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
        response = requests.patch(
            "{}/{}".format(SMARTRECRUITERS_PROFILES_ENDPOINT, profile["id"]),
            headers={"x-smarttoken": auth_parameters.x_smart_token},
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update profile to SmartRecruiters profile_id={}"
                " status_code={} response={}".format(
                    profile["id"],
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def delete_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        response = requests.delete(
            "{}/{}".format(SMARTRECRUITERS_PROFILES_ENDPOINT, profile["id"]),
            headers={"x-smarttoken": auth_parameters.x_smart_token},
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to delete profile from SmartRecruiters profile_id={}"
                " status_code={} response={}".format(
                    profile["id"],
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


JobsAisle = Aisle(
    name=Entity.job,
    schema=SmartRecruitersJob,
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
        # endpoints=Endpoints(
        #     create=GET_JOB_ENDPOINT,
        # ),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=SmartRecruitersProfile,
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
        endpoints=Endpoints(
            create=GET_CANDIDATE_ENDPOINT,
            update=GET_CANDIDATE_ENDPOINT,
            archive=GET_CANDIDATE_ENDPOINT,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=WriteProfilesParameters,
            archive=WriteProfilesParameters,
        ),
        function=merge(
            create=write_profiles,
            update=update_profiles,
            archive=delete_profiles,
        ),
        endpoints=Endpoints(
            create=POST_CANDIDATE_ENDPOINT,
            update=UPDATE_CANDIDATE_ENDPOINT,
            archive=DELETE_CANDIDATE_ENDPOINT,
        ),
    ),
)
