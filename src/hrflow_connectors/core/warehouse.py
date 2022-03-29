import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel, Field


class ActionEndpoints(BaseModel):
    name: str
    description: str
    url: str


class WarehouseReadAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[[LoggerAdapter, BaseModel], t.Iterable[t.Dict]]

    def __call__(self, *args, **kwargs) -> t.Iterable[t.Dict]:
        return self.function(*args, **kwargs)


class WarehouseWriteAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[[LoggerAdapter, BaseModel, t.Iterable[t.Dict]], t.List[t.Dict]]

    def __call__(self, *args, **kwargs) -> t.List[t.Dict]:
        return self.function(*args, **kwargs)


class Warehouse(BaseModel):
    name: str
    data_schema: t.Type[BaseModel] = Field(default_factory=lambda: BaseModel)
    read: t.Optional[WarehouseReadAction]
    write: t.Optional[WarehouseWriteAction]

    @property
    def is_readable(self):
        return self.read is not None

    @property
    def is_writable(self):
        return self.write is not None
