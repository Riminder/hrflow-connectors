import base64
import binascii
import typing as t
from logging import LoggerAdapter

from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.jobijoba.schemas import JobApplication
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class AuthParameters(Struct):
    pass


class ReadProfilesParameters(Struct):
    profile: Annotated[
        t.Dict,
        Meta(
            description="Event object recieved from the Webhook",
        ),
    ]


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    result = {**parameters.profile}
    resume = result.get("resume") or {}
    file = resume.get("file") or {}
    cv_base64 = file.get("data") or None

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
    content_type = file.get("contentType") or None
    result["cv"] = binary_data
    result["content_type"] = content_type

    return [result]


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=JobApplication,
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
