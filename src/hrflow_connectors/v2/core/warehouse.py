import typing as t
from dataclasses import dataclass, field
from logging import LoggerAdapter

from msgspec import Struct

from hrflow_connectors.v2.core.common import Entity, Mode, Parameters, Schema


class SpecificRead(t.Protocol):
    def __call__(
        self,
        adapter: LoggerAdapter,
        auth_parameters: t.Any,
        parameters: t.Any,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[dict]:
        ...  # pragma: nocover


class Read(t.Protocol):
    def __call__(
        self,
        *,
        mode: Mode,
        adapter: LoggerAdapter,
        auth_parameters: t.Any,
        parameters: t.Any,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[dict]:
        ...  # pragma: nocover


class SpecificWrite(t.Protocol):
    def __call__(
        self,
        adapter: LoggerAdapter,
        auth_parameters: t.Any,
        parameters: t.Any,
        items: t.Iterable[dict],
    ) -> list[dict]:
        ...  # pragma: nocover


class Write(t.Protocol):
    def __call__(
        self,
        *,
        mode: Mode,
        adapter: LoggerAdapter,
        auth_parameters: t.Any,
        parameters: t.Any,
        items: t.Iterable[dict],
    ) -> list[dict]:
        ...  # pragma: nocover


class ModeIsNotSupported(Exception):
    pass


@t.overload
def merge(
    *,
    create: t.Optional[SpecificRead] = None,
    update: t.Optional[SpecificRead] = None,
    archive: t.Optional[SpecificRead] = None,
) -> Read:
    ...  # pragma: nocover


@t.overload
def merge(
    *,
    create: t.Optional[SpecificWrite] = None,
    update: t.Optional[SpecificWrite] = None,
    archive: t.Optional[SpecificWrite] = None,
) -> Write:
    ...  # pragma: nocover


def merge(
    *,
    create: t.Optional[t.Callable[..., t.Any]] = None,
    update: t.Optional[t.Callable[..., t.Any]] = None,
    archive: t.Optional[t.Callable[..., t.Any]] = None,
) -> t.Callable[..., t.Any]:
    def merged(mode: Mode, **kwargs: t.Any) -> list[dict]:
        if mode is Mode.create and create is not None:
            return create(**kwargs)

        if mode is Mode.update and update is not None:
            return update(**kwargs)

        if mode is Mode.archive and archive is not None:
            return archive(**kwargs)

        raise ModeIsNotSupported(f"{mode} mode is not supported")

    return merged


@dataclass
class Endpoint:
    name: str
    description: str
    url: str


@dataclass
class Criterias:
    create: t.Optional[Schema] = None
    update: t.Optional[Schema] = None
    archive: t.Optional[Schema] = None

    def parameters(self, mode: Mode):
        if mode is Mode.create:
            return self.create
        if mode is Mode.update:
            return self.update
        if mode is Mode.archive:
            return self.archive


@dataclass
class Endpoinsts:
    create: t.Optional[Endpoint] = None
    update: t.Optional[Endpoint] = None
    archive: t.Optional[Endpoint] = None

    def for_mode(self, mode: Mode):
        if mode is Mode.create:
            return self.create
        if mode is Mode.update:
            return self.update
        if mode is Mode.archive:
            return self.archive


OperationT = t.TypeVar("OperationT", Read, Write)


@dataclass
class Operation(t.Generic[OperationT]):
    function: OperationT
    criterias: Criterias
    endpoints: Endpoinsts = field(default_factory=Endpoinsts)


@dataclass
class ReadOperation(Operation[Read]):
    get_incremental_token: t.Optional[t.Callable[[dict], str]] = None

    @property
    def supports_incremental(self):
        return self.get_incremental_token is not None

    def __call__(
        self,
        *,
        mode: Mode,
        adapter: LoggerAdapter,
        auth_parameters: Parameters,
        parameters: Parameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ):
        return self.function(
            mode=mode,
            adapter=adapter,
            auth_parameters=auth_parameters,
            parameters=parameters,
            incremental=incremental,
            incremental_token=incremental_token,
        )


@dataclass
class WriteOperation(Operation[Write]):
    def __call__(
        self,
        *,
        mode: Mode,
        adapter: LoggerAdapter,
        auth_parameters: Parameters,
        parameters: Parameters,
        items: list[dict],
    ):
        return self.function(
            mode=mode,
            adapter=adapter,
            auth_parameters=auth_parameters,
            parameters=parameters,
            items=items,
        )


@dataclass
class Aisle:
    name: Entity
    read: t.Optional[ReadOperation] = None
    write: t.Optional[WriteOperation] = None
    schema: Schema = field(default_factory=lambda: Struct)

    def parameters(self, operation: t.Literal["read", "write"], mode: Mode):
        if operation == "read" and self.read is not None:
            return self.read.criterias.parameters(mode)
        elif operation == "write" and self.write is not None:
            return self.write.criterias.parameters(mode)


@dataclass
class Warehouse:
    auth: Schema
    aisles: tuple[Aisle, ...]

    def get_aisle(self, entity: Entity) -> t.Optional[Aisle]:
        return self.__dict__.setdefault(
            "__aisle_by_entity__", {aisle.name: aisle for aisle in self.aisles}
        ).get(entity)
