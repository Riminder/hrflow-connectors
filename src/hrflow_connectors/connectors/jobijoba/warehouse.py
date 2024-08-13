import base64
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
    resume = result.get("resume") or {}
    file = resume.get("file") or {}
    cv_base64 = file.get("data") or None

    if cv_base64 is None:
        raise ValueError("No base64 string provided for CV.")

    try:
        binary_data = base64.b64decode(cv_base64)
    except base64.binascii.Error:
        padding_needed = 4 - (len(cv_base64) % 4)
        if padding_needed != 4:
            cv_base64 += "=" * padding_needed
            binary_data = base64.b64decode(cv_base64)

    if not binary_data:
        raise Exception("Error decoding base64 string")
    content_type = file.get("contentType") or None
    result["cv"] = binary_data
    result["content_type"] = content_type

    return [result]


JobijobaProfilesWarehouse = Warehouse(
    name="Jobijoba Candidate",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)
