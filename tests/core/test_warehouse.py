import pytest
from pydantic import BaseModel, Field, PositiveInt, ValidationError

from hrflow_connectors.core.warehouse import (
    DataType,
    FieldNotFoundError,
    FixedValueValidationError,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class Parameters(BaseModel):
    age: int = Field(..., gt=0)
    distance: PositiveInt
    name: str


TestWarehouse = Warehouse(
    name="Test Warehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=Parameters, function=lambda *args, **kwargs: None
    ),
    write=WarehouseWriteAction(
        parameters=Parameters, function=lambda *args, **kwargs: None
    ),
)


def test_with_fixed_parameters_field_does_not_exist():
    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_read_parameters(age=20, does_not_exist="xxx")

    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_write_parameters(age=20, does_not_exist="xxx")


def test_with_fixed_parameters_bad_value():
    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(name=[1, 2])

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_write_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_write_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_write_parameters(name=[1, 2])


def test_with_fixed_read_parameters():
    fixed = TestWarehouse.with_fixed_read_parameters(age=22)
    assert fixed.read.parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_read_parameters(age=22, distance=200)
    assert fixed.read.parameters(name="test").age == 22
    assert fixed.read.parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_read_parameters(age=22, distance=200, name="fixed")
    assert fixed.read.parameters().age == 22
    assert fixed.read.parameters().distance == 200
    assert fixed.read.parameters().name == "fixed"


def test_with_fixed_write_parameters():
    fixed = TestWarehouse.with_fixed_write_parameters(age=22)
    assert fixed.write.parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_write_parameters(age=22, distance=200)
    assert fixed.write.parameters(name="test").age == 22
    assert fixed.write.parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_write_parameters(
        age=22, distance=200, name="fixed"
    )
    assert fixed.write.parameters().age == 22
    assert fixed.write.parameters().distance == 200
    assert fixed.write.parameters().name == "fixed"


def test_read_action_incremental_validation():
    with pytest.raises(ValidationError) as excinfo:
        WarehouseReadAction(
            parameters=Parameters,
            function=lambda *args, **kwargs: None,
            supports_incremental=True,
        )
    assert "item_to_read_from must be provided" in excinfo.value.errors()[0]["msg"]
