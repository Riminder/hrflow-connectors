import enum
import json
import logging
import os
import typing as t
from pathlib import Path

from pydantic import BaseModel

logger = logging.getLogger(__name__)
store = None
is_configured = False

ENABLE_STORE_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE_ENABLED"
STORE_TYPE_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE"


class StoreType(enum.Enum):
    localjson = enum.auto()


class LocalJsonStore:
    DIRECTORY_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_LOCALJSON_DIR"
    STORE_FILENAME = "store.json"

    def __init__(
        self,
    ) -> None:
        self.type = StoreType.localjson
        directory = os.environ.get(LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE, None)
        if directory is None:
            raise Exception(
                "Missing environment variable {} in"
                " order to setup LocalJson store".format(
                    LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE
                )
            )
        directory = Path(directory)
        if directory.is_absolute() is False:
            raise Exception(
                "{}={} should be an absolute filepath".format(
                    LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE, directory
                )
            )
        if directory.exists() is False:
            raise Exception(
                "{}={} does not exist".format(
                    LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE, directory
                )
            )
        self.store_fd = directory / LocalJsonStore.STORE_FILENAME
        if self.store_fd.exists() is False:
            self.store_fd.write_text(LocalJsonStore.dumps(self.empty_store()))
        else:
            try:
                json.loads(self.store_fd.read_text())
            except (json.JSONDecodeError, UnicodeDecodeError):
                raise Exception("Store file is corrupted. Unable to JSON decode")

    def empty_store(self) -> t.Dict:
        return dict(root="HrFlow Connectors", store="LocalJson", data=dict())

    @staticmethod
    def dumps(data: t.Any) -> str:
        return json.dumps(data, indent=2)

    def save(self, key: str, data: BaseModel) -> None:
        store = json.loads(self.store_fd.read_text())
        store["data"][key] = data.json()
        self.store_fd.write_text(LocalJsonStore.dumps(store))
        return None

    def load(self, key: str, parse_as: t.Type[BaseModel]) -> BaseModel:
        store = json.loads(self.store_fd.read_text())
        if key in store["data"]:
            return parse_as.parse_raw(store["data"][key])
        return None


TYPE_TO_STORE = {StoreType.localjson: LocalJsonStore}


def configure_store():
    global store, is_configured

    enable_store = os.environ.get(ENABLE_STORE_ENVIRONMENT_VARIABLE, None)
    if not enable_store or enable_store in ["false", "False", "0"]:
        store = None
        is_configured = False
        return

    store_type = os.environ.get(
        STORE_TYPE_ENVIRONMENT_VARIABLE, StoreType.localjson.name
    )
    try:
        store_type = StoreType[store_type]
    except KeyError:
        raise Exception(
            "{}='{}' is not a valid store use one of {}".format(
                STORE_TYPE_ENVIRONMENT_VARIABLE,
                store_type,
                [type.name for type in StoreType],
            )
        )

    store = TYPE_TO_STORE[store_type]()
    is_configured = True
