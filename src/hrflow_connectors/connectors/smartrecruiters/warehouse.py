import enum
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.smartrecruiters.schemas import (
    SmartRecruitersJob,
    SmartRecruitersProfile,
)
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

SMARTRECRUITERS_JOBS_ENDPOINT = "https://api.smartrecruiters.com/jobs"
SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT = 100

GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description=(
        "Endpoint to search jobs by traditional params (offset, limit...)"
        " and get the list of all jobs with their ids, the request method"
        " is `GET`"
    ),
    url=(
        "https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.all"
    ),
)
GET_JOB_ENDPOINT = ActionEndpoints(
    name="Get job",
    description=(
        "Endpoint to get the content of a job with a given id, a jobId parameter is"
        " required, the request method is `GET`"
    ),
    url=(
        "https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.get"
    ),
)
POST_CANDIDATE_ENDPOINT = ActionEndpoints(
    name="Post Candidate",
    description=(
        "Endpoint to create a new candidate and assign to a talent pool, the request"
        " method is `POST`"
    ),
    url="https://dev.smartrecruiters.com/customer-api/live-docs/candidate-api/",
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


class WriteProfilesParameters(ParametersModel):
    x_smart_token: str = Field(
        ...,
        description="X-SmartToken used to access SmartRecruiters API",
        repr=False,
        field_type=FieldType.Auth,
    )
    job_id: str = Field(
        ...,
        description=(
            "Id of a Job to which you want to assign a candidates "
            "when it’s created. Profiles are sent to this "
            "URL `https://api.smartrecruiters.com/jobs/{job_id}/candidates` "
        ),
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(ParametersModel):
    x_smart_token: str = Field(
        ...,
        description="X-SmartToken used to access SmartRecruiters API",
        repr=False,
        field_type=FieldType.Auth,
    )
    query: t.Optional[str] = Field(
        None,
        description=(
            "Case insensitive full-text query against job title e.g. java developer"
        ),
        field_type=FieldType.QueryParam,
    )
    updated_after: t.Optional[str] = Field(
        None,
        description="ISO8601-formatted time boundaries for the job update time",
        field_type=FieldType.QueryParam,
    )
    posting_status: t.Optional[JobPostingStatus] = Field(
        None,
        description="Posting status of a job. One of {}".format(
            [e.value for e in JobPostingStatus]
        ),
        field_type=FieldType.QueryParam,
    )
    job_status: t.Optional[JobStatus] = Field(
        None,
        description="Status of a job. One of {}".format([e.value for e in JobStatus]),
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT,
        description=(
            "Number of items to pull from SmartRecruiters at a time. Not matter what"
            " value is supplied it is capped at {}".format(
                SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT
            )
        ),
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    page = None
    while True:
        params = dict(
            q=parameters.query,
            updatedAfter=parameters.updated_after,
            postingStatus=parameters.posting_status,
            status=parameters.job_status,
            limit=min(parameters.limit, SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT),
            pageId=page,
        )
        response = requests.get(
            SMARTRECRUITERS_JOBS_ENDPOINT,
            headers={"X-SmartToken": parameters.x_smart_token},
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
        jobs = response["content"]
        if len(jobs) == 0:
            break

        adapter.info(
            "Pulling {} jobs from page {} out of total jobs {}".format(
                len(jobs), page if page is not None else 1, response["totalFound"]
            )
        )
        for job in jobs:
            full_job_response = requests.get(
                "{}/{}".format(SMARTRECRUITERS_JOBS_ENDPOINT, job["id"]),
                headers={"X-SmartToken": parameters.x_smart_token},
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

        page = response["nextPageId"]
        if not page:
            break


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info(
        "Pushing {} profiles with job_id={}".format(len(profiles), parameters.job_id)
    )
    failed_profiles = []
    for profile in profiles:
        response = requests.post(
            "{}/{}/candidates".format(SMARTRECRUITERS_JOBS_ENDPOINT, parameters.job_id),
            headers={"X-SmartToken": parameters.x_smart_token},
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to SmartRecruiters job_id={}"
                " status_code={} response={}".format(
                    parameters.job_id,
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


SmartRecruitersJobWarehouse = Warehouse(
    name="SmartRecruiters Jobs",
    data_schema=SmartRecruitersJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[GET_ALL_JOBS_ENDPOINT, GET_JOB_ENDPOINT],
    ),
)

SmartRecruitersProfileWarehouse = Warehouse(
    name="SmartRecruiters Profiles",
    data_schema=SmartRecruitersProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
)
