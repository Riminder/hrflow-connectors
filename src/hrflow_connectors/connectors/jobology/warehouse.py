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


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    result = {**parameters.profile}
    cv_url = result["cvUrl"]
    response = requests.get(cv_url)
    if response.status_code == 200:
        result["cv"] = response.content
        result["content_type"] = response.headers["Content-Type"]
    elif response.status_code == 400:
        raise Exception(f"Bad Request {response.text}")
    else:
        raise Exception(
            f"request failed with status code {response.status_code} and message"
            f" {response.text}"
        )

    return [result]


JobologyProfilesWarehouse = Warehouse(
    name="Jobology Candidate",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)
