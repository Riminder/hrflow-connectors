from hrflow_connectors.v2.core.common import Mode, Entity, Schema
from hrflow_connectors.v2.core.warehouse import (
    merge,
    ModeIsNotSupported,
    Aisle,
    ReadOperation,
    WriteOperation,
    Criterias,
    Warehouse,
)
import typing as t

import pytest

from msgspec import Struct
from unittest.mock import MagicMock


@pytest.mark.parametrize("running_with_mode", list(Mode))
def test_merge_is_working_as_expected(running_with_mode: Mode):
    functions = dict()
    for mode in list(Mode):
        if mode is running_with_mode:
            functions[mode.value] = MagicMock(return_value="success")
        else:
            functions[mode.value] = MagicMock(
                side_effect=Exception("Should not be called")
            )

    merged = merge(**functions)

    assert merged(running_with_mode) == "success"


@pytest.mark.parametrize("running_with_mode", list(Mode))
def test_merged_raises_for_unsupported_mode(running_with_mode: Mode):
    functions = dict()
    for mode in list(Mode):
        if mode is not running_with_mode:
            functions[mode.value] = MagicMock(return_value="success")

    merged = merge(**functions)

    with pytest.raises(ModeIsNotSupported):
        merged(running_with_mode)


def test_read_supports_incremental():
    assert (
        ReadOperation(
            function=MagicMock(),
            criterias=Criterias(),
        ).supports_incremental
        is False
    )

    assert (
        ReadOperation(
            function=MagicMock(),
            criterias=Criterias(),
            get_incremental_token=lambda *args, **kwargs: "token",
        ).supports_incremental
        is True
    )


def test_get_aisle_parameters():
    parameters = dict(read=dict(), write=dict())
    for operation in ["read", "write"]:
        for mode in list(Mode):
            parameters[operation][mode.value] = MagicMock()

    TestAisle = Aisle(
        name=Entity.profile,
        read=ReadOperation(
            function=MagicMock(), criterias=Criterias(**parameters["read"])
        ),
        write=WriteOperation(
            function=MagicMock(), criterias=Criterias(**parameters["write"])
        ),
        schema=Struct,
    )

    for operation in ["read", "write"]:
        assert operation == "read" or operation == "write"

        for mode in list(Mode):
            assert (
                TestAisle.parameters(operation, mode)
                is parameters[operation][mode.value]
            )


def test_get_aisle_parameters_return_none():
    CreateWrite = MagicMock()
    ArchiveWrite = MagicMock()

    TestAisle = Aisle(
        name=Entity.profile,
        read=ReadOperation(function=MagicMock(), criterias=Criterias()),
        write=WriteOperation(
            function=MagicMock(),
            criterias=Criterias(
                create=t.cast(Schema, CreateWrite), archive=t.cast(Schema, ArchiveWrite)
            ),
        ),
        schema=Struct,
    )

    assert TestAisle.parameters("read", Mode.create) is None
    assert TestAisle.parameters("read", Mode.update) is None
    assert TestAisle.parameters("read", Mode.archive) is None

    assert TestAisle.parameters("write", Mode.create) is CreateWrite
    assert TestAisle.parameters("write", Mode.update) is None
    assert TestAisle.parameters("write", Mode.archive) is ArchiveWrite


def test_wareshouse_get_aisle():
    # See https://docs.python.org/3.9/library/unittest.mock.html?highlight=magicmock#mock-names-and-the-name-attribute
    # for why MagicMock(name=Entity.job) doesn't work
    JobAisle = MagicMock()
    JobAisle.name = Entity.job

    ProfileAisle = MagicMock()
    ProfileAisle.name = Entity.profile

    TestWarehouse = Warehouse(
        auth=MagicMock,
        aisles=(
            JobAisle,
            ProfileAisle,
        ),
    )

    assert TestWarehouse.get_aisle(Entity.job) is JobAisle
    assert TestWarehouse.get_aisle(Entity.profile) is ProfileAisle
    assert TestWarehouse.get_aisle(Entity.application) is None
