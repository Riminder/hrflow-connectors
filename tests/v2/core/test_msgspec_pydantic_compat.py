from hrflow_connectors.v2.core.msgspec_pydantic_compat import serialize, ValidationError
import pytest

from typing_extensions import Annotated

from pydantic import BaseModel, Field
from msgspec import Struct, Meta


class PydanticModel(BaseModel):
    name: str
    age: int = 10
    limit: int = Field(..., lt=100)


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
