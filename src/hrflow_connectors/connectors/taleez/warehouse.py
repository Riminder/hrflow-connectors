# Code from PR: https://github.com/Riminder/hrflow-connectors/pull/66
# Author: Yasser BELMEHDI (yass1337)
import enum
import json
import re
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.taleez.schemas import Candidate, Job
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
    url="https://api.taleez.com/0/jobs",
)


class JobStatus(str, enum.Enum):
    published = "PUBLISHED"


class ReadJobsParameters(ParametersModel):
    x_taleez_api_secret: str = Field(
        ...,
        description="X-taleez-api-secret used to access Taleez API",
        repr=False,
        field_type=FieldType.Auth,
    )
    with_details: bool = Field(..., description="xxx", field_type=FieldType.QueryParam)
    job_status: JobStatus = Field(
        None,
        description="Posting status of a job. One of {}".format(
            [e.value for e in JobStatus]
        ),
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    params = dict(withDetails=parameters.with_details, status=parameters.job_status)

    response = requests.get(
        TALEEZ_JOBS_ENDPOINT,
        headers={"X-taleez-api-secret": parameters.x_taleez_api_secret},
        params=params,
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


class WriteProfilesParameters(ParametersModel):
    accept: str = Field(
        ACCEPT, const=True, field_type=FieldType.QueryParam
    )  # TODO: Include Headers in FieldType
    x_taleez_api_secret: str = Field(
        ...,
        description="Client Secret id used to access Taleez API",
        repr=False,
        field_type=FieldType.Auth,
    )
    content_type: str = Field(
        ..., description="Content type", repr=True, field_type=FieldType.QueryParam
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
        headers = {
            "accept": "{}".format(parameters.accept),
            "X-taleez-api-secret": "{}".format(parameters.x_taleez_api_secret),
            "Content-Type": "{}".format(parameters.content_type),
        }

        response = requests.post(
            POST_CANDIDATE_ENDPOINT,
            headers=headers,
            data=json.dumps(profile["candidate"]),
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create candidate status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
        id = int(re.search("[0-9]{7,8}", str(response.content)).group(0))
        binary_cv = requests.get(profile["CV"]).content
        response = requests.post(
            "{}/{}/documents?cv=true".format(POST_CANDIDATE_ENDPOINT, id),
            headers={
                "accept": "{}".format(parameters.accept),
                "X-taleez-api-secret": "{}".format(parameters.x_taleez_api_secret),
            },
            files=[
                (
                    "file",
                    (
                        "CV_{}_{}".format(
                            profile["candidate"]["firstName"],
                            profile["candidate"]["lastName"],
                        ),
                        binary_cv,
                        "application/pdf",
                    ),
                )
            ],
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add document status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        if profile["properties"]:
            response = requests.post(
                "{}/{}/properties".format(POST_CANDIDATE_ENDPOINT, id),
                data=json.dumps(profile["properties"]),
                headers={
                    "accept": "{}".format(parameters.accept),
                    "X-taleez-api-secret": "{}".format(parameters.x_taleez_api_secret),
                    "Content-Type": "{}".format(parameters.content_type),
                },
            )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update property status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


TaleezProfilesWarehouse = Warehouse(
    name="Taleez Profiles Warehouse",
    data_schema=Candidate,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
    ),
)
TaleezJobWarehouse = Warehouse(
    name="Taleez Jobs Warehouse",
    data_schema=Job,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[GET_ALL_JOBS_ENDPOINT],
    ),
)
