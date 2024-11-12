import pytest
from msgspec import Meta, Struct
from pydantic import BaseModel, Field
from typing_extensions import Annotated

from hrflow_connectors.v2.core.msgspec_pydantic_compat import (
    ValidationError,
    fields,
    serialize,
)


class PydanticModel(BaseModel):
    name: str
    limit: int = Field(..., lt=100)
    age: int = 10


class MsgSpecModel(Struct):
    name: str
    limit: Annotated[int, Meta(lt=100)]
    age: int = 10


def test_serialize_pydantic_valid_data():
    serialized = serialize(dict(name="test", limit=99), PydanticModel)

    assert serialized.name == "test"
    assert serialized.age == 10
    assert serialized.limit == 99


def test_serialize_msgspec_valid_data():
    serialized = serialize(dict(name="test", limit=99), MsgSpecModel)

    assert serialized.name == "test"
    assert serialized.age == 10
    assert serialized.limit == 99


BAD_DATA = (
    dict(name=[1, 2, 3], limit=99),
    dict(name="test", age=99, limit=100),
)


@pytest.mark.parametrize("data", BAD_DATA)
def test_serialize_pydantic_bad_data(data: dict):
    with pytest.raises(ValidationError):
        serialize(data, PydanticModel)


@pytest.mark.parametrize("data", BAD_DATA)
def test_serialize_msgspec_bad_data(data: dict):
    with pytest.raises(ValidationError):
        serialize(data, MsgSpecModel)


def test_fields_working_as_expected():
    expecting = ("name", "limit", "age")

    assert fields(PydanticModel) == expecting
    assert fields(MsgSpecModel) == expecting
