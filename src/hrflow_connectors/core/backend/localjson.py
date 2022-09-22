import json
import os
import typing as t
from pathlib import Path

from pydantic import BaseModel

from hrflow_connectors.core.backend.common import BackendStore


class LocalJsonStore(BackendStore):
    DIRECTORY_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_LOCALJSON_DIR"
    STORE_FILENAME = "store.json"

    def __init__(
        self,
    ) -> None:
        directory = os.environ.get(LocalJsonStore.DIRECTORY_ENVIRONMENT_VARIABLE)
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
            self.store_fd.write_text(LocalJsonStore.dumps(LocalJsonStore.empty_store()))
        else:
            try:
                json.loads(self.store_fd.read_text())
            except (json.JSONDecodeError, UnicodeDecodeError):
                raise Exception("Store file is corrupted. Unable to JSON decode")

    @staticmethod
    def NAME() -> str:
        return "localjson"

    @staticmethod
    def empty_store() -> t.Dict:
        return dict(root="HrFlow Connectors", store=LocalJsonStore.NAME(), data=dict())

    @staticmethod
    def dumps(data: t.Any) -> str:
        return json.dumps(data, indent=2)

    def save(self, key: str, data: BaseModel) -> None:
        store = json.loads(self.store_fd.read_text())
        store["data"][key] = data.json()
        self.store_fd.write_text(LocalJsonStore.dumps(store))
        return None

    def load(self, key: str, parse_as: t.Type[BaseModel]) -> t.Optional[BaseModel]:
        store = json.loads(self.store_fd.read_text())
        if key in store["data"]:
            return parse_as.parse_raw(store["data"][key])
        return None
