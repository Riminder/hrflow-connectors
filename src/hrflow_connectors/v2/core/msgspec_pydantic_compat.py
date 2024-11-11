import typing as t

from pydantic import BaseModel, ValidationError as PydanticValidationError
from msgspec import Struct, convert, ValidationError as MsgSpecValidationError

from hrflow_connectors.v2.core.common import Schema, Parameters

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
