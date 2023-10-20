import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.freshteam.schemas import JobPostings
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)

jobs_url = "https://{account_name}.freshteam.com/api/job_postings"


class BaseParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="api key",
        repr=False,
        field_type=FieldType.Auth,
    )
    account_name: str = Field(
        ...,
        description="account name",
        repr=False,
        field_type=FieldType.Auth,
    )
    job_id: int = Field(
        ...,
        description="job id",
        repr=False,
        field_type=FieldType.Auth,
    )
    limit: int = Field(
        50,
        description="limit",
        repr=False,
        field_type=FieldType.Auth,
    )
    sort_type: str = Field(
        "asc",
        description="sort type",
        repr=False,
        field_type=FieldType.Auth,
    )
    sort: str = Field(
        "created_at",
        description="sort",
        repr=False,
        field_type=FieldType.Auth,
    )


def read_jobs(
    adapter: LoggerAdapter,
    parameters: BaseParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    adapter.info("Fetching jobs from freshteam")
    jobs_list_url = jobs_url.format(account_name=parameters.account_name)

    if parameters.api_key is None:
        raise Exception("Authentication failed")
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {parameters.api_key}",
    }
    params = {
        "limit": parameters.limit,
        "id": parameters.job_id,
        "sort_type": parameters.sort_type,
        "sort": parameters.sort,
    }
    while True:
        response = requests.get(jobs_list_url, headers=headers, params=params)
        if response.status_code == 200:
            result = response.json()
            if not result["link"]:
                break
            for job in result:
                yield job
            jobs_list_url = result["link"]
        else:
            raise Exception("Error in fetching jobs")


FreshTeamJobWarehouse = Warehouse(
    name="FreshTeam Jobs",
    data_schema=JobPostings,
    data_type=DataType.job,
    read=WarehouseReadAction(parameters=BaseParameters, function=read_jobs),
)
