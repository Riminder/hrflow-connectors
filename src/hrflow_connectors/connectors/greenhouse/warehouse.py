import base64
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.greenhouse.schemas import (
    GreenhouseJobModel,
    GreenhouseProfileModel,
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

GET_JOB_ENDPOINT = ActionEndpoints(
    name="Get job",
    description=(
        "Endpoint to get the content of a job with a given id."
        " The request method is `GET`"
    ),
    url="https://developers.greenhouse.io/harvest.html?shell#get-retrieve-job",
)
POST_CANDIDATE_ENDPOINT = ActionEndpoints(
    name="Post Candidate",
    description=(
        "Endpoint to create a new candidate and assign to a talent pool, the request"
        " method is `POST`"
    ),
    url="https://developers.greenhouse.io/job-board.html#jobs",
)


class WriteProfilesParameters(ParametersModel):
    auth: str = Field(..., description="XAPIKeyAuth", field_type=FieldType.Auth)
    on_behalf_of: str = Field(
        ...,
        description=(
            "The ID of the user sending the profile, or the person he is sending the"
            " profile on behalf of"
        ),
        field_type=FieldType.QueryParam,
    )


class ReadProfilesParameters(ParametersModel):
    auth: str = Field(..., description="XAPIKeyAuth", field_type=FieldType.Auth)
    created_after: str = Field(
        None,
        description=(
            "Return only candidates that were created at or after this timestamp."
            " Timestamp must be in in ISO-8601 format."
        ),
        field_type=FieldType.QueryParam,
    )
    updated_after: str = Field(
        None,
        description=(
            "Return only candidates that were updated at or after this timestamp."
            " Timestamp must be in in ISO-8601 format."
        ),
        field_type=FieldType.QueryParam,
    )
    job_id: str = Field(
        None,
        description=(
            "If supplied, only return candidates that have applied to this job. Will"
            " return both when a candidate has applied to a job and when they’re a"
            " prospect for a job."
        ),
        field_type=FieldType.QueryParam,
    )
    email: str = Field(
        None,
        description=(
            "If supplied, only return candidates who have a matching e-mail address. If"
            " supplied with job_id, only return a candidate with a matching e-mail with"
            " an application on the job. If email and candidate_ids are included,"
            " candidate_ids will be ignored."
        ),
        field_type=FieldType.QueryParam,
    )
    candidate_ids: str = Field(
        None,
        description=(
            "If supplied, only return candidates with matching ids. If supplied with"
            " job_id, only return a candidate with a matching id with an application on"
            " the job. If email and candidate_ids are included, candidate_ids will be"
            " ignored."
        ),
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(ParametersModel):
    board_token: str = Field(
        ..., description="Board_token", field_type=FieldType.QueryParam
    )


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    parameters.auth = parameters.auth + ":"
    authorization = base64.b64encode(parameters.auth.encode("ascii"))
    params = parameters.dict()
    del params["auth"]
    response = requests.get(
        "https://harvest.greenhouse.io/v1/candidates",
        headers={
            "Authorization": b"Basic " + authorization,
        },
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull profiles from Greenhouse params={}"
            " status_code={} response={}".format(
                parameters, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull profiles from Greenhouse")
    response = response.json()
    profiles = response["candidates"]
    adapter.info("Pulling {} profiles".format(len(profiles)))
    for profile in profiles:
        yield profile


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    while True:
        response = requests.get(
            "https://boards-api.greenhouse.io/v1/boards/{}/jobs/?content=true".format(
                parameters.board_token
            ),
            headers={},
            params=None,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Greenhouse params={}"
                " status_code={} response={}".format(
                    parameters, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Greenhouse")
        response = response.json()
        jobs = response["jobs"]
        if len(jobs) == 0:
            break

        adapter.info("Pulling {} jobs".format(len(jobs)))
        for job in jobs:
            yield job
        break


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []
    for profile in profiles:
        parameters.auth = parameters.auth + ":"
        authorization = base64.b64encode(parameters.auth.encode("ascii"))
        response = requests.post(
            "https://harvest.greenhouse.io/v1/candidates",
            headers={
                "On-Behalf-Of": parameters.on_behalf_of,
                "Authorization": b"Basic " + authorization,
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Greenhouse"
                " status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


GreenhouseJobWarehouse = Warehouse(
    name="Greenhouse Jobs",
    data_schema=GreenhouseJobModel,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[GET_JOB_ENDPOINT],
    ),
)

GreenhouseProfileWarehouse = Warehouse(
    name="Greenhouse Profiles",
    data_schema=GreenhouseProfileModel,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
    ),
)
