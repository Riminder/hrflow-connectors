import typing as t
from enum import Enum

from msgspec import Struct
from pydantic import BaseModel

Schema = t.Union[type[BaseModel], type[Struct]]
Parameters = t.Union[Struct, BaseModel]


class Entity(Enum):
    job = "job"
    profile = "profile"
    application = "application"


class Mode(Enum):
    create = "create"
    update = "update"
    archive = "archive"


class Direction(Enum):
    inbound = "inbound"
    outbound = "outbound"
