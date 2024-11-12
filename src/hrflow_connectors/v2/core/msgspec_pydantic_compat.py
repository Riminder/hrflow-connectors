import typing as t

from msgspec import Struct, convert, json
from msgspec import ValidationError as MsgSpecValidationError
from pydantic import BaseModel
from pydantic import ValidationError as PydanticValidationError

from hrflow_connectors.v2.core.common import Parameters, Schema

T = t.TypeVar("T", bound=t.Union[Struct, BaseModel])


class ValidationError(Exception):
    pass


@t.overload
def serialize(obj: dict, schema: type[T]) -> T: ...
@t.overload
def serialize(obj: dict, schema: Schema) -> Parameters: ...
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


def json_schema(schema: Schema, unwrap: bool = True) -> dict:
    if issubclass(schema, BaseModel):
        return schema.schema()

    wrapped = json.schema(schema)
    if unwrap:
        path = wrapped["$ref"].rsplit("/", 1)[-1]
        return wrapped["$defs"][path]
    return wrapped
