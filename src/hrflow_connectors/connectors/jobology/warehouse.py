import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)


class ReadProfilesParameters(ParametersModel):
    profile: t.Dict = Field(
        None,
        description="Event object recieved from the Webhook",
        field_type=FieldType.Other,
    )


class ErrorHandler(Exception):
    pass


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    cv_url = parameters.profile["cvUrl"]
    response = requests.get(cv_url)
    response.raise_for_status()
    parameters.profile["cv"] = response.content
    parameters.profile["content_type"] = response.headers["Content-Type"]
    cv_url = parameters.profile["cvUrl"]
    response = requests.get(cv_url)
    # Check for specific HTTP status codes and handle accordingly
    if response.status_code == 200:
        parameters.profile["cv"] = response.content
        parameters.profile["content_type"] = response.headers["Content-Type"]
    elif response.status_code == 400:
        raise ErrorHandler(f"Error 400: Bad Request - {response.text}")
    else:
        raise ErrorHandler(f"Unexpected HTTP status code: {response.status_code}")

    return [parameters.profile]


JobologyProfilesWarehouse = Warehouse(
    name="Jobology Candidate",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)
