import typing as t
from logging import LoggerAdapter

from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.waalaxy.schemas import WaalaxyProfile
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class AuthParameters(Struct):
    pass


class ReadProfilesParameters(Struct):
    profile: Annotated[
        t.Dict,
        Meta(
            description="Profile object recieved from the Webhook",
        ),
    ]


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    return [parameters.profile]


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=WaalaxyProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(create=read, update=read, archive=read),
    ),
)
