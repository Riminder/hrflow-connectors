import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.jazzhr.schemas import JazzHrApplicants, JazzHrJobs
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

JOBS_URL = "https://api.resumatorapi.com/v1/jobs"
APPLICANTS_URL = "https://api.resumatorapi.com/v1/applicants"


class BaseParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="The API key to authenticate with JazzHR",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(BaseParameters):
    pass


class ReadApplicantsParameters(BaseParameters):
    pass


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    adapter.debug("Reading jobs from JazzHR")
    # TODO: Recheck how to authenticate with JazzHR
    # TODO: Check if the API key is the only way to authenticate
    # TODO: Check for pagination
    headers = {"Authorization": f"Basic {parameters.api_key}"}
    response = requests.get(JOBS_URL, headers=headers)
    if response.status_code == 200:
        jobs = response.json()
        for job in jobs:
            yield job
    else:
        raise Exception(f"Failed to read jobs from JazzHR: {response.text}")


def read_applicants(
    adapter: LoggerAdapter,
    parameters: ReadApplicantsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    adapter.debug("Reading applicants from JazzHR")
    # TODO: Recheck how to authenticate with JazzHR
    # TODO: Check if the API key is the only way to authenticate
    # TODO: Check for pagination
    headers = {"Authorization": f"Basic {parameters.api_key}"}
    response = requests.get(APPLICANTS_URL, headers=headers)
    if response.status_code == 200:
        applicants = response.json()
        for applicant in applicants:
            yield applicant
    else:
        raise Exception(f"Failed to read applicants from JazzHR: {response.text}")


JazzhrApplicantWarehouse = Warehouse(
    name="Jazzhr Profiles",
    data_schema=JazzHrApplicants,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadApplicantsParameters,
        function=read_applicants,
    ),
    write=WarehouseWriteAction(
        parameters=ReadApplicantsParameters,
        function=read_applicants,
    ),
)

JazzhrJobWarehouse = Warehouse(
    name="Jazzhr Jobs",
    data_schema=JazzHrJobs,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)
