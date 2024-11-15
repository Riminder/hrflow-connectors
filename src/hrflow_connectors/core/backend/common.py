import typing as t
from collections import Counter
from enum import Enum

from msgspec import Struct
from pydantic import BaseModel


# This decoder add support for Counter[AnyEnum] which is a
# specialized dict used in RunResult
# See https://jcristharif.com/msgspec/extending.html#mapping-to-from-native-types
def msgspec_dec_hook(type: type, obj: t.Any) -> t.Any:
    if (
        t.get_origin(type) is Counter
        and len(t.get_args(type)) == 1
        and issubclass((EnumModel := t.get_args(type)[0]), Enum)
    ):
        return Counter({EnumModel(key): value for key, value in obj.items()})
    else:
        raise NotImplementedError(
            f"Objects of type {type} are not supported"
        )  # pragma: nocover


class StoreNotInitializedError(Exception):
    pass


InternalStateT = t.TypeVar("InternalStateT", contravariant=True)


class SaveP(t.Protocol[InternalStateT]):
    def __call__(
        self, state: InternalStateT, key: str, data: t.Union[BaseModel, Struct]
    ) -> None:
        ...  # pragma: nocover


class LoadP(t.Protocol[InternalStateT]):
    def __call__(
        self,
        state: InternalStateT,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ) -> t.Union[BaseModel, Struct, None]:
        ...  # pragma: nocover


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
    def load(self, key: str, parse_as: type[ParseAsT]) -> t.Optional[ParseAsT]:
        ...  # pragma: nocover

    @t.overload
    def load(
        self,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ) -> t.Union[BaseModel, Struct, None]:
        ...  # pragma: nocover

    def load(
        self,
        key: str,
        parse_as: t.Union[type[BaseModel], type[Struct]],
    ):
        if self.state is None:
            raise StoreNotInitializedError("Backend not initialized: call store.init()")
        return self.loader(self.state, key, parse_as)
