import base64
import binascii
import typing as t
from io import BytesIO
from logging import LoggerAdapter

import puremagic
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.meteojob.schemas import MeteojobEventObject
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class AuthParameters(Struct):
    pass


class ReadProfilesParameters(Struct):
    profile: Annotated[
        MeteojobEventObject,
        Meta(
            description="Event object recieved from the Webhook",
        ),
    ]


def get_content_type(binary_data: bytes):
    inferred = "application/octet-stream"

    try:
        results = puremagic.magic_stream(BytesIO(binary_data))
        if len(results) > 0:
            inferred = results[0].mime_type
    except puremagic.PureError:
        pass

    return inferred


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    result = asdict(parameters.profile)
    cv_base64 = result.get("cvBase64")

    if cv_base64 is None:
        raise ValueError("No base64 string provided for CV.")

    try:
        binary_data = base64.b64decode(cv_base64, validate=True)
    except binascii.Error:
        padding_needed = len(cv_base64) % 4
        if padding_needed > 0:
            cv_base64 += "=" * (4 - padding_needed)
            try:
                binary_data = base64.b64decode(cv_base64, validate=True)
            except binascii.Error:
                raise Exception("Invalid Base64 string provided for CV.")

    if not binary_data:
        raise Exception("Error decoding base64 string")
    content_type = get_content_type(binary_data)
    result["cv"] = binary_data
    result["content_type"] = content_type

    return [result]


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=MeteojobEventObject,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read,
            update=read,
            archive=read,
        ),
    ),
)
