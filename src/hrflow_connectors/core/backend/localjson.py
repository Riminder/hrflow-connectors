import os
import typing as t
from pathlib import Path

from msgspec import DecodeError, Struct, convert, json
from pydantic import BaseModel

from hrflow_connectors.core.backend.common import BackendStore, msgspec_dec_hook

DIRECTORY_ENVIRONMENT_VARIABLE: t.Final = "HRFLOW_CONNECTORS_LOCALJSON_DIR"
STORE_FILENAME: t.Final = "store.json"
NAME: t.Final = "localjson"


class InternalState(Struct):
    path: Path


def empty_store() -> t.Dict:
    return dict(root="HrFlow Connectors", store=NAME, data=dict())


def get_state():
    directory = os.environ.get(DIRECTORY_ENVIRONMENT_VARIABLE)
    if directory is None:
        raise Exception(
            "Missing environment variable {} in order to setup LocalJson store".format(
                DIRECTORY_ENVIRONMENT_VARIABLE
            )
        )
    directory = Path(directory)
    if directory.is_absolute() is False:
        raise Exception(
            "{}={} should be an absolute filepath".format(
                DIRECTORY_ENVIRONMENT_VARIABLE, directory
            )
        )
    if directory.exists() is False:
        raise Exception(
            "{}={} does not exist".format(DIRECTORY_ENVIRONMENT_VARIABLE, directory)
        )
    path = directory / STORE_FILENAME
    if path.exists() is False:
        path.write_bytes(json.encode(empty_store()))
    else:
        try:
            json.decode(path.read_text())
        except (DecodeError, UnicodeDecodeError):
            raise Exception("Store file is corrupted. Unable to JSON decode")
    return InternalState(path=path)


def save(state: InternalState, key: str, data: t.Union[BaseModel, Struct]) -> None:
    store = json.decode(state.path.read_bytes())
    store["data"][key] = (
        data.json() if isinstance(data, BaseModel) else json.encode(data).decode()
    )
    state.path.write_bytes(json.encode(store))


def load(
    state: InternalState, key: str, parse_as: t.Union[type[BaseModel], type[Struct]]
):
    store = json.decode(state.path.read_bytes())
    if key in store["data"]:
        if issubclass(parse_as, BaseModel):
            return parse_as.parse_raw(store["data"][key])
        return convert(
            json.decode(store["data"][key]), parse_as, dec_hook=msgspec_dec_hook
        )
    return None


LocalJsonStore = BackendStore[InternalState](
    name=NAME, get_state=get_state, saver=save, loader=load
)
