import json
import typing as t
from json import JSONDecodeError
from logging import LoggerAdapter
from pathlib import Path

from pydantic import BaseModel, Field, FilePath

from hrflow_connectors.core import Warehouse, WarehouseReadAction, WarehouseWriteAction


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")


def read(adapter: LoggerAdapter, parameters: ReadJsonParameters) -> t.Iterable[t.Dict]:
    # Because of validation happening in ReadJsonParameters
    # no need to handle FileNotFoundError
    try:
        with open(parameters.path, "r") as f:
            data = json.load(f)
    except JSONDecodeError as e:
        message = "Invalid JSON file. Failed to decode with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)

    if isinstance(data, list):
        for item in data:
            yield item
    else:
        yield data


class WriteJsonParameters(BaseModel):
    path: Path = Field(..., description="Path where to save JSON file")


def write(
    adapter: LoggerAdapter, parameters: WriteJsonParameters, items: t.Iterator[t.Dict]
) -> None:
    items = list(items)
    try:
        with open(parameters.path, "w") as f:
            json.dump(items, f)
    # More error handling can be added to cope with file permissions for example
    except TypeError as e:
        message = "Failed to JSON encode provided items with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)


LocalJSONWarehouse = Warehouse(
    name="LocalJSONWarehouse",
    read=WarehouseReadAction(
        parameters=ReadJsonParameters,
        function=read,
    ),
    write=WarehouseWriteAction(
        parameters=WriteJsonParameters,
        function=write,
    ),
)
