import typing as t
from enum import Enum

import pytest
from msgspec import Meta, Struct
from pydantic import BaseModel, Field
from typing_extensions import Annotated

from hrflow_connectors.v2.core.common import Schema
from hrflow_connectors.v2.core.msgspec_pydantic_compat import (
    ValidationError,
    fields,
    json_schema,
    serialize,
    template_fields,
)


class Gender(Enum):
    M = "male"
    F = "female"


class PydanticLocation(BaseModel):
    city: str


class PydanticModel(BaseModel):
    name: str
    limit: int = Field(..., lt=100, description="Description for limit")
    age: int = 10
    location: PydanticLocation
    gender: Gender = Gender.M
    container: t.Union[dict, list, int, float] = 1


class MsgSpecLocation(Struct):
    city: str


class MsgSpecModel(Struct, kw_only=True):
    name: str
    limit: Annotated[int, Meta(lt=100, description="Description for limit")]
    age: int = 10
    location: MsgSpecLocation
    gender: Gender = Gender.M
    container: t.Union[dict, list, int, float] = 1


def test_serialize_pydantic_valid_data():
    serialized = serialize(
        dict(name="test", limit=99, location=dict(city="Casablanca")), PydanticModel
    )

    assert serialized.name == "test"
    assert serialized.age == 10
    assert serialized.limit == 99
    assert serialized.location.city == "Casablanca"


def test_serialize_msgspec_valid_data():
    serialized = serialize(
        dict(name="test", limit=99, location=dict(city="Casablanca")), MsgSpecModel
    )

    assert serialized.name == "test"
    assert serialized.age == 10
    assert serialized.limit == 99
    assert serialized.location.city == "Casablanca"


BAD_DATA = (
    dict(name=[1, 2, 3], limit=99),
    dict(name="test", age=99, limit=100),
    dict(name="test", age=5, limit=50),
    dict(name="test", age=5, limit=50, location=True),
    dict(name="test", age=5, limit=50, location=dict(city=None)),
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
    expecting = ("name", "limit", "age", "location", "gender", "container")

    assert fields(PydanticModel) == expecting
    assert fields(MsgSpecModel) == expecting


def test_json_schema_working_as_expected():
    assert isinstance(json_schema(MsgSpecModel, unwrap=True), dict)
    assert isinstance(json_schema(MsgSpecModel, unwrap=False), dict)
    assert isinstance(json_schema(MsgSpecModel), dict)
    assert isinstance(json_schema(PydanticModel), dict)

    assert json_schema(MsgSpecModel) == json_schema(MsgSpecModel, unwrap=True)


@pytest.mark.parametrize("Model", [MsgSpecModel, PydanticModel])
def test_template_fields_works_as_expected(Model: Schema):
    [
        name_field,
        limit_field,
        age_field,
        location_field,
        gender_field,
        container_field,
    ] = template_fields(Model)

    assert name_field.name == "name"
    assert name_field.type == "string"
    assert name_field.required is True
    assert name_field.description == ""
    assert name_field.default is None

    assert limit_field.name == "limit"
    assert limit_field.type == "integer"
    assert limit_field.required is True
    assert limit_field.description == "Description for limit"
    assert limit_field.default is None

    assert age_field.name == "age"
    assert age_field.type == "integer"
    assert age_field.required is False
    assert age_field.description == ""
    assert age_field.default == 10

    assert location_field.name == "location"
    assert location_field.type == "object"
    assert location_field.required is True
    assert location_field.description == ""
    assert location_field.default is None

    assert gender_field.name == "gender"
    assert (
        gender_field.type == "Literal['male','female']"
        or gender_field.type == "Literal['female','male']"
    )
    assert gender_field.required is False
    assert gender_field.description == ""
    assert gender_field.default is Gender.M.value

    assert container_field.name == "container"
    assert container_field.type == "object|array|integer|number"
    assert container_field.required is False
    assert container_field.description == ""
    assert container_field.default == 1
