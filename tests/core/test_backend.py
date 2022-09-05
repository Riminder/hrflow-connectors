import os
import typing as t
from unittest import mock

import pytest
from pydantic import BaseModel

from hrflow_connectors.core import backend


@pytest.fixture
def backend_restore():
    yield
    backend.configure_store()


def test_store_disabled(backend_restore):
    for v in ["False", "false", "0"]:
        with mock.patch.dict(
            os.environ, {backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: v}
        ):
            backend.configure_store()
            assert backend.is_configured is False
            assert backend.store is None


def test_bad_store_name(backend_restore):
    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "NotValid",
        },
    ):
        with pytest.raises(Exception) as excinfo:
            backend.configure_store()
        assert "not a valid store" in excinfo.value.args[0]


class TestModel(BaseModel):
    key1: str
    key2: int
    key3: t.Dict


def test_localjson_store_bad_configuration(backend_restore):
    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
        },
        clear=True,
    ):
        with pytest.raises(Exception) as excinfo:
            backend.configure_store()
        assert "Missing environment variable" in excinfo.value.args[0]

    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
            backend.LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE: "./ubuntu",
        },
    ):
        with pytest.raises(Exception) as excinfo:
            backend.configure_store()
        assert "should be an absolute filepath" in excinfo.value.args[0]

    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
            backend.LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE: (
                "/home/userDoesNotExist/work"
            ),
        },
    ):
        with pytest.raises(Exception) as excinfo:
            backend.configure_store()
        assert "does not exist" in excinfo.value.args[0]


def test_localjson_store(backend_restore, tmp_path):
    key = "xxx_TestLocalJson"
    data = TestModel(key1="xxx", key2=3, key3=dict(test=True))

    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
            backend.LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE: str(tmp_path),
        },
    ):
        backend.configure_store()

        assert backend.store.load(key, TestModel) is None
        backend.store.save(key, data)
        assert backend.store.load(key, TestModel) == data

    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "0",
        },
    ):
        backend.configure_store()
        assert backend.store is None

    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
            backend.LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE: str(tmp_path),
        },
    ):
        backend.configure_store()

        assert backend.store.load(key, TestModel) == data


def test_localjson_store_corrupted_file(backend_restore, tmp_path):
    corrupted_store = tmp_path / backend.LocalJsonStore.STORE_FILENAME
    corrupted_store.write_bytes(0xFF.to_bytes(4, "big"))
    with mock.patch.dict(
        os.environ,
        {
            backend.ENABLE_STORE_ENVIRONMENT_VARIABLE: "1",
            backend.STORE_NAME_ENVIRONMENT_VARIABLE: "localjson",
            backend.LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE: str(tmp_path),
        },
    ):
        with pytest.raises(Exception) as excinfo:
            backend.configure_store()

        print(excinfo)
        assert "Store file is corrupted" in excinfo.value.args[0]
