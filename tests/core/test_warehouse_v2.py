import pytest
from pydantic import Field, PositiveInt, ValidationError

from hrflow_connectors.core.warehouse_v2 import (
    BadFieldTypeError,
    DataType,
    FieldNotFoundError,
    FieldType,
    FixedValueValidationError,
    InvalidFieldError,
    NoFieldTypeError,
    ParametersModel,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class AuthParameters(ParametersModel):
    client_id: str = Field(
        ...,
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        repr=False,
        field_type=FieldType.Auth,
    )


class Parameters(ParametersModel):
    age: int = Field(..., gt=0, field_type=FieldType.Other)
    distance: PositiveInt = Field(..., field_type=FieldType.Other)
    name: str = Field(..., field_type=FieldType.Other)


TestWarehouse = Warehouse(
    name="Test Warehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=Parameters,
        function=lambda *args, **kwargs: None,
    ),
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=Parameters,
        function=lambda *args, **kwargs: None,
    ),
    update=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=Parameters,
        function=lambda *args, **kwargs: None,
    ),
    archive=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=Parameters,
        function=lambda *args, **kwargs: None,
    ),
)


def test_with_fixed_parameters_field_does_not_exist():
    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_read_parameters(age=20, does_not_exist="xxx")

    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_create_parameters(age=20, does_not_exist="xxx")

    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_update_parameters(age=20, does_not_exist="xxx")

    with pytest.raises(FieldNotFoundError):
        TestWarehouse.with_fixed_archive_parameters(age=20, does_not_exist="xxx")


def test_with_fixed_parameters_bad_value():
    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_read_parameters(name=[1, 2])

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_create_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_create_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_create_parameters(name=[1, 2])

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_update_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_update_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_update_parameters(name=[1, 2])

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_archive_parameters(age=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_archive_parameters(distance=-2)

    with pytest.raises(FixedValueValidationError):
        TestWarehouse.with_fixed_archive_parameters(name=[1, 2])


def test_with_fixed_read_parameters():
    fixed = TestWarehouse.with_fixed_read_parameters(age=22)
    assert fixed.read.action_parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_read_parameters(age=22, distance=200)
    assert fixed.read.action_parameters(name="test").age == 22
    assert fixed.read.action_parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_read_parameters(age=22, distance=200, name="fixed")
    assert fixed.read.action_parameters().age == 22
    assert fixed.read.action_parameters().distance == 200
    assert fixed.read.action_parameters().name == "fixed"


def test_with_fixed_create_parameters():
    fixed = TestWarehouse.with_fixed_create_parameters(age=22)
    assert fixed.create.action_parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_create_parameters(age=22, distance=200)
    assert fixed.create.action_parameters(name="test").age == 22
    assert fixed.create.action_parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_create_parameters(
        age=22, distance=200, name="fixed"
    )
    assert fixed.create.action_parameters().age == 22
    assert fixed.create.action_parameters().distance == 200
    assert fixed.create.action_parameters().name == "fixed"


def test_with_fixed_update_parameters():
    fixed = TestWarehouse.with_fixed_update_parameters(age=22)
    assert fixed.update.action_parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_update_parameters(age=22, distance=200)
    assert fixed.update.action_parameters(name="test").age == 22
    assert fixed.update.action_parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_update_parameters(
        age=22, distance=200, name="fixed"
    )
    assert fixed.update.action_parameters().age == 22
    assert fixed.update.action_parameters().distance == 200
    assert fixed.update.action_parameters().name == "fixed"


def test_with_fixed_archive_parameters():
    fixed = TestWarehouse.with_fixed_archive_parameters(age=22)
    assert fixed.archive.action_parameters(distance=100, name="test").age == 22

    fixed = TestWarehouse.with_fixed_archive_parameters(age=22, distance=200)
    assert fixed.archive.action_parameters(name="test").age == 22
    assert fixed.archive.action_parameters(name="test").distance == 200

    fixed = TestWarehouse.with_fixed_archive_parameters(
        age=22, distance=200, name="fixed"
    )
    assert fixed.archive.action_parameters().age == 22
    assert fixed.archive.action_parameters().distance == 200
    assert fixed.archive.action_parameters().name == "fixed"


def test_read_action_incremental_validation():
    with pytest.raises(ValidationError) as excinfo:
        WarehouseReadAction(
            auth_parameters=AuthParameters,
            action_parameters=Parameters,
            function=lambda *args, **kwargs: None,
            supports_incremental=True,
        )
    assert "item_to_read_from must be provided" in excinfo.value.errors()[0]["msg"]


def test_field_type_validation():
    with pytest.raises(InvalidFieldError):

        class MyParameters1(ParametersModel):
            name: str

    with pytest.raises(NoFieldTypeError):

        class MyParameters2(ParametersModel):
            name: str = Field(...)

    with pytest.raises(BadFieldTypeError):

        class MyParameters3(ParametersModel):
            name: str = Field(..., field_type="Auth")


def test_all_auth_parameters_have_right_field_type():
    for field in AuthParameters.__fields__.values():
        assert field.field_info.extra["field_type"] == FieldType.Auth
