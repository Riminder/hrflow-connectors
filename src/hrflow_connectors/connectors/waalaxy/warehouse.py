import typing as t
from logging import LoggerAdapter

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
        description="Profile object recieved from the Webhook",
        field_type=FieldType.Other,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    return [parameters.profile]


WaalaxyProfilesWarehouse = Warehouse(
    name="Waalaxy Profiles",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)
