import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.ceridian.schemas import CeridianDayforceJobModel
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)


class ReadJobsParameters(ParametersModel):
    subdomain: str = Field(
        ...,
        description="Subdomain used to access Ceridian API",
        repr=False,
        field_type=FieldType.Other,
    )

    client_name_space: str = Field(
        ...,
        description="Client name space used to access Ceridian API",
        repr=False,
        field_type=FieldType.Other,
    )

    companyName: t.Optional[str] = Field(
        None,
        description="Company name. Example: XYZ Co.",
        field_type=FieldType.QueryParam,
    )
    parentCompanyName: t.Optional[str] = Field(
        None,
        description="Parent Company name. Example: Ceridian",
        field_type=FieldType.QueryParam,
    )

    lastUpdateTimeFrom: t.Optional[str] = Field(
        None,
        description=(
            "A starting timestamp of job posting date. Example: 2017-01-01T13:24:56"
        ),
        field_type=FieldType.QueryParam,
    )

    htmlDescription: t.Optional[bool] = Field(
        None,
        description=(
            "A flag to feed the jobs over with html formatting or plain text"
            " description"
        ),
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    params = dict()
    params["companyName"] = parameters.companyName
    params["parentCompanyName"] = parameters.parentCompanyName
    params["lastUpdateTimeFrom"] = parameters.lastUpdateTimeFrom
    params["htmlDescription"] = parameters.htmlDescription

    url = "https://{}.dayforcehcm.com/Api/{}/V1/JobFeeds".format(
        parameters.subdomain, parameters.client_name_space
    )

    response = requests.request(
        "GET",
        url=url,
        headers={},
        data={},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from Ceridian params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull jobs from Ceridian")
    response = response.json()
    return response


CeridianJobWarehouse = Warehouse(
    name="Ceridian Jobs",
    data_schema=CeridianDayforceJobModel,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[],
    ),
)
