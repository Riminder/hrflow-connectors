import collections
import collections.abc
import typing as t
from dataclasses import dataclass
from functools import reduce

from msgspec import Struct
from msgspec import ValidationError as MsgSpecValidationError
from msgspec import convert, json
from pydantic import BaseModel
from pydantic import ValidationError as PydanticValidationError

from hrflow_connectors.v2.core.common import Parameters, Schema

T = t.TypeVar("T", bound=t.Union[Struct, BaseModel])


class ValidationError(Exception):
    pass


@t.overload
def serialize(obj: dict, schema: type[T]) -> T:
    ...  # pragma: nocover


@t.overload
def serialize(obj: dict, schema: Schema) -> Parameters:
    ...  # pragma: nocover


def serialize(obj: dict, schema: Schema) -> Parameters:
    if issubclass(schema, BaseModel):
        try:
            return schema(**obj)
        except PydanticValidationError as e:
            raise ValidationError(e.errors())
    try:
        return convert(obj, schema)
    except MsgSpecValidationError as e:
        raise ValidationError(*e.args)


def fields(schema: Schema) -> tuple[str, ...]:
    if issubclass(schema, BaseModel):
        return tuple(schema.__fields__)
    return schema.__struct_fields__


def msgspec_schema_hook(t: type):
    if issubclass(t, collections.abc.Callable):
        return dict(type="Callable")
    raise NotImplementedError  # pragma: nocover


def json_schema(schema: Schema, unwrap: bool = True) -> dict:
    if issubclass(schema, BaseModel):
        return schema.schema()

    wrapped = json.schema(schema, schema_hook=msgspec_schema_hook)
    if unwrap:
        path = wrapped.pop("$ref").rsplit("/", 1)[-1]
        unwrapped = wrapped["$defs"].pop(path)
        return {**unwrapped, "$defs": wrapped["$defs"]}
    return wrapped


@dataclass
class TemplateField:
    name: str
    type: str
    required: bool
    description: str
    default: str


def get_type(definition: dict, json_schema: dict) -> str:
    if type := definition.get("type"):
        return type

    if (
        sub_definitions := definition.get("anyOf", definition.get("allOf"))
    ) is not None:
        return "|".join(
            [
                get_type(sub_definition, json_schema)
                for sub_definition in t.cast(list[dict], sub_definitions)
            ]
        )
    if ref := definition.get("$ref"):
        return get_type(
            reduce(
                lambda reduced, path: reduced[path],
                ref.strip("#/").split("/"),
                json_schema,
            ),
            json_schema,
        )
    if choices := definition.get("enum"):
        return "Literal[" + ",".join([f"'{choice}'" for choice in choices]) + "]"

    return ""  # pragma: nocover


def template_fields(schema: Schema) -> list[TemplateField]:
    _json_schema = json_schema(schema)
    return [
        TemplateField(
            name=key,
            type=get_type(definition, _json_schema),
            required=key in _json_schema["required"],
            description=definition.get("description", ""),
            default=definition.get("default", None),
        )
        for key, definition in _json_schema["properties"].items()
    ]
