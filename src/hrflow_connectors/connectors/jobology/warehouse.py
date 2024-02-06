import base64
import typing as t
from logging import LoggerAdapter

import magic
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


class Base64DecodeError(Exception):
    pass


class ContentTypeDetectionError(Exception):
    pass


def get_content_type(binary_data: bytes):
    try:
        mime = magic.Magic(mime=True)
        content_type = mime.from_buffer(binary_data)
        return content_type
    except Exception as e:
        raise ContentTypeDetectionError(f"Error detecting content type: {e}")


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    result = {**parameters.profile}
    cv_base64 = result.get("cvBase64")

    if cv_base64 is None:
        raise ValueError("No base64 string provided for CV.")

    try:
        binary_data = base64.b64decode(cv_base64)
    except Exception as e:
        raise Base64DecodeError(f"Error decoding base64 string: {e}")

    try:
        content_type = get_content_type(binary_data)
    except ContentTypeDetectionError as e:
        adapter.error(e)
        content_type = "application/octet-stream"

    result["cv"] = binary_data
    result["content_type"] = content_type

    return [result]


JobologyProfilesWarehouse = Warehouse(
    name="Jobology Candidate",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)
