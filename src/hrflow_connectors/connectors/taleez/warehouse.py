import enum
from logging import LoggerAdapter
import typing as t
import re

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import (Warehouse, WarehouseWriteAction, WarehouseReadAction, ActionEndpoints)
from hrflow_connectors.connectors.taleez.schemas import (Candidate, Job)


POST_CANDIDATE_ENDPOINT = "https://api.taleez.com/0/candidates"
TALEEZ_JOBS_ENDPOINT = "https://api.taleez.com/0/jobs"
TALEEZ_JOBS_ENDPOINT_LIMIT = 100
ACCEPT = "application/json;charset=UTF-8"
CONTENT_TYPE = "application/json"
GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description=(
        "Endpoint to retrieve all jobs."
        " and get the list of all jobs with their ids, the request method"
        " is `GET`"
    ),
    url=(
        "https://api.taleez.com/0/jobs"
    ),
)


class JobStatus(str, enum.Enum):
    published = "PUBLISHED"


class PullJobsParameters(BaseModel):
    x_taleez_api_secret: str = Field(
        ..., description="X-taleez-api-secret used to access Taleez API", repr=False
    )
    with_details: bool = Field(..., description="xxx")
    job_status: JobStatus = Field(None, description="Posting status of a job. One of {}".format(
        [e.value for e in JobStatus]
    ))


def read(adapter: LoggerAdapter, parameters: PullJobsParameters) -> t.Iterable[t.Dict]:
    params = dict(
        withDetails=parameters.with_details,
        status=parameters.job_status
    )

    response = requests.get(
        TALEEZ_JOBS_ENDPOINT,
        headers={ "X-taleez-api-secret": parameters.x_taleez_api_secret },
        params=params
    )

    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from Taleez params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull jobs from Taleez")

    response = response.json()
    jobs = response["list"]

    for job in jobs:
        yield job


class WriteProfilesParameters(BaseModel):
    accept: str = Field(ACCEPT, const=True)
    X_taleez_api_secret: str = Field(
        ..., description="Client Secret id used to access Taleez API", repr=False
    )


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    data: t.Iterable[t.Dict],
) -> t.Iterable[t.Dict]:
    data = list(data)
    adapter.info("Pushing {} candidates to Taleez API".format(len(data)))
    failed_profiles = []
    for profile in data:
        response = requests.post(
            POST_CANDIDATE_ENDPOINT, json=profile["candidate"], headers=parameters
        )
        id = re.search("[0-9]{7,8}", response.content).group(0)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # TODO: request to add documents and properties to candidate
        binary_cv = requests.get(profile["CV"]).content
        response = requests.post(
            "{}/{}/documents?cv=true".format(POST_CANDIDATE_ENDPOINT, id),
            files=[("file", (binary_cv, "application/pdf"))],
            headers=parameters,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # TODO: POST the properties to candidate after having created it
        response = requests.post(
            "{}/{}/properties".format(POST_CANDIDATE_ENDPOINT, id),
            json=profile["properties"],
            headers=parameters,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # properties = get_profile_properties_to_push()

    return failed_profiles


TaleezProfilesWarehouse = Warehouse(
    name="Taleez Profiles Warehouse",
    data_schema=Candidate,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
    ),
)
TaleezJobWarehouse = Warehouse(
    name="Taleez Jobs Warehouse",
    data_schema=Job,
    read=WarehouseReadAction(
        parameters=PullJobsParameters,
        function=read,
        endpoints=[GET_ALL_JOBS_ENDPOINT],
    ),
)
