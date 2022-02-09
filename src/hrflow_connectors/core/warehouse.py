import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel, Field


class ActionEndpoints(BaseModel):
    name: str
    description: str
    url: str


class WarehousePullAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[[LoggerAdapter, BaseModel], t.Iterable[t.Dict]]

    def __call__(self, *args, **kwargs) -> t.Iterable[t.Dict]:
        return self.function(*args, **kwargs)


class WarehousePushAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[[LoggerAdapter, BaseModel, t.Iterable[t.Dict]], None]

    def __call__(self, *args, **kwargs) -> None:
        self.function(*args, **kwargs)


class Warehouse(BaseModel):
    name: str
    data_schema: t.Type[BaseModel]
    pull: t.Optional[WarehousePullAction]
    push: t.Optional[WarehousePushAction]

    @property
    def is_pullable(self):
        return self.pull is not None

    @property
    def is_pushable(self):
        return self.push is not None
