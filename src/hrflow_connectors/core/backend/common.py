import typing as t

from pydantic import BaseModel
from msgspec import Struct


class StoreNotInitializedError(Exception):
    pass


InternalStateT = t.TypeVar("InternalStateT", contravariant=True)


class SaveP(t.Protocol[InternalStateT]):
    def __call__(
        self, state: InternalStateT, key: str, data: t.Union[BaseModel, Struct]
    ) -> None: ...


class LoadP(t.Protocol[InternalStateT]):
    def __call__(
        self,
        state: InternalStateT,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ) -> t.Union[BaseModel, Struct, None]: ...


class BackendStore(Struct, t.Generic[InternalStateT]):
    name: str
    get_state: t.Callable[[], InternalStateT]
    saver: SaveP[InternalStateT]
    loader: LoadP[InternalStateT]
    state: t.Optional[InternalStateT] = None

    def init(self):
        self.state = self.get_state()

    def save(self, key: str, data: t.Union[BaseModel, Struct]):
        if self.state is None:
            raise StoreNotInitializedError("Backend not initialized: call store.init()")
        return self.saver(self.state, key, data)

    ParseAsT = t.TypeVar("ParseAsT", bound=t.Union[BaseModel, Struct])

    @t.overload
    def load(self, key: str, parse_as: type[ParseAsT]) -> t.Optional[ParseAsT]: ...
    @t.overload
    def load(
        self,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ) -> t.Union[BaseModel, Struct, None]: ...
    def load(
        self,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ):
        if self.state is None:
            raise StoreNotInitializedError("Backend not initialized: call store.init()")
        return self.loader(self.state, key, parse_as)
