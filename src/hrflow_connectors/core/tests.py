import json
import os
import typing as t
from contextlib import contextmanager
from contextvars import ContextVar
from importlib import import_module
from pathlib import Path

import yaml
from pydantic import BaseModel, Field, StrictStr, ValidationError

from hrflow_connectors.core.connector import ActionName as ActionNameEnum
from hrflow_connectors.core.connector import Connector, Event, Reason, Status
from hrflow_connectors.core.warehouse import ReadMode, Warehouse

PROJECT_DIRECTORY = Path(__file__).parent.parent.parent.parent
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"

SECRETS_PREFIX = "$__"
ENVIRON_SECRETS_PREFIX = "HRFLOW_CONNECTORS_{connector_name}_"
Secrets = ContextVar("secrets")
ConnectorName = ContextVar("connector_name")
ActionNames = ContextVar("action_names")
WarehouseNames = ContextVar("warehouse_names")


class NoTestConfigException(Exception):
    pass


class InvalidJSONException(Exception):
    pass


class InvalidYAMLException(Exception):
    pass


class InvalidTestConfigException(Exception):
    pass


@contextmanager
def secrets(connector_name: str, connectors_directory: Path):
    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=connector_name.upper()
    )

    global_secrets_file = connectors_directory / "secrets.json"
    if global_secrets_file.exists():
        try:
            global_secrets = json.loads(global_secrets_file.read_text())
            global_secrets = {
                key.replace(secrets_prefix, "", 1): value
                for key, value in global_secrets.items()
                if key.startswith(secrets_prefix)
            }
        except json.JSONDecodeError as e:
            raise InvalidJSONException(
                "Failed to JSON decode global secrets file for connector {} with"
                " error {}".format(connector_name, e)
            )
    else:
        global_secrets = dict()

    connector_secrets_file = (
        connectors_directory / connector_name.lower() / "secrets.json"
    )
    if connector_secrets_file.exists():
        try:
            connector_secrets = json.loads(connector_secrets_file.read_text())
        except json.JSONDecodeError as e:
            raise InvalidJSONException(
                "Failed to JSON decode secrets file for connector {} "
                "with error {}".format(connector_name, e)
            )
    else:
        connector_secrets = dict()

    environnment_secrets = {
        key.replace(secrets_prefix, "", 1): value
        for key, value in os.environ.items()
        if key.startswith(secrets_prefix)
    }
    token = Secrets.set({**global_secrets, **connector_secrets, **environnment_secrets})
    yield
    Secrets.reset(token)


@contextmanager
def actions(connector: Connector):
    connector_name_token = ConnectorName.set(connector.model.name)
    actions_token = ActionNames.set([action.name for action in connector.model.actions])
    yield
    ConnectorName.reset(connector_name_token)
    ActionNames.reset(actions_token)


@contextmanager
def warehouses(connector_name: str, connectors_directory: Path):
    if connectors_directory is CONNECTORS_DIRECTORY:  # pragma: no cover
        warehouse_module = import_module(
            "hrflow_connectors.connectors.{}.warehouse".format(connector_name.lower())
        )
    else:
        import_from = connectors_directory.relative_to(PROJECT_DIRECTORY)
        warehouse_module = import_module(
            "{}.{}.warehouse".format(
                str(import_from).replace("/", "."), connector_name.lower()
            )
        )
    warehouse_names = []
    for attribute, value in warehouse_module.__dict__.items():
        if isinstance(value, Warehouse):
            warehouse_names.append(attribute)
    token = WarehouseNames.set(warehouse_names)
    yield
    WarehouseNames.reset(token)


class ParameterValue(object):
    @classmethod
    def validate(cls, v):
        if isinstance(v, str) and v.startswith(SECRETS_PREFIX):
            key = v.replace(SECRETS_PREFIX, "", 1)
            if key not in Secrets.get(dict()):
                raise TypeError(
                    "{} not found in secrets for connector {}".format(
                        key, ConnectorName.get("")
                    )
                )
            return Secrets.get()[key]
        return v

    @classmethod
    def __get_validators__(cls):
        yield cls.validate

    @classmethod
    def __modify_schema__(cls, field_schema):
        field_schema.update(
            description=(
                "If the value is a string starting with {} then it's fetched from"
                " environnment and secrets.json file".format(SECRETS_PREFIX)
            ),
            examples=["$API_KEY"],
        )


class ActionName(StrictStr):
    @classmethod
    def validate(cls, v):
        try:
            ActionNameEnum[v]
        except KeyError:
            raise TypeError(
                "'{}' is not a valid action name. Should be one of {}".format(
                    v, [name.value for name in ActionNameEnum]
                )
            )
        action_names = ActionNames.get([])
        if v not in action_names:
            raise TypeError(
                "No action '{}' found for connector {}. Should be one of {}".format(
                    v, ConnectorName.get(""), action_names
                )
            )
        return v

    @classmethod
    def __get_validators__(cls):
        for validator in super().__get_validators__():
            yield validator
        yield cls.validate


class WarehouseName(StrictStr):
    @classmethod
    def validate(cls, v):
        warehoue_names = WarehouseNames.get([])
        if v not in warehoue_names:
            raise TypeError(
                "Warehouse '{}' not found for connector {}. Should be one of {}".format(
                    v, ConnectorName.get(""), warehoue_names
                )
            )
        return v

    @classmethod
    def __get_validators__(cls):
        for validator in super().__get_validators__():
            yield validator
        yield cls.validate


class ReadTest(BaseModel):
    id: t.Optional[str] = None
    parameters: t.Dict[str, ParameterValue]
    read_mode: t.Optional[ReadMode] = None
    read_from: t.Optional[str] = None
    expected_number_of_items: t.Optional[int] = None


class WarehouseTests(BaseModel):
    read: t.List[ReadTest] = Field(default_factory=list)


class ActionTest(BaseModel):
    id: t.Optional[str] = None
    origin_parameters: t.Dict[str, ParameterValue]
    target_parameters: t.Dict[str, ParameterValue]
    status: t.Optional[Status]
    reason: t.Optional[Reason]
    events: t.Optional[t.Counter[Event]]


class ConnectorTestConfig(BaseModel):
    warehouse: t.Dict[WarehouseName, WarehouseTests] = Field(default_factory=dict)
    actions: t.Dict[ActionName, t.List[ActionTest]] = Field(default_factory=dict)


def collect_connector_tests(
    connector: Connector, connectors_directory: Path = CONNECTORS_DIRECTORY
):
    connector_name = connector.model.name
    test_config_file = (
        connectors_directory / connector_name.lower() / "test-config.yaml"
    )
    if test_config_file.exists() is False:
        raise NoTestConfigException(
            "No test configuration found for connector {} at {}".format(
                connector_name, test_config_file
            )
        )
    try:
        test_config = yaml.safe_load(test_config_file.read_bytes())
    except yaml.YAMLError as e:
        raise InvalidYAMLException(
            "Failed to parse yaml test config for connector {} with error {} ".format(
                connector_name, str(e)
            )
        )

    try:
        with warehouses(
            connector_name=connector_name,
            connectors_directory=connectors_directory,
        ):
            with actions(
                connector=connector,
            ):
                with secrets(
                    connector_name=connector_name,
                    connectors_directory=connectors_directory,
                ):
                    test_suite = ConnectorTestConfig(**test_config)
    except ValidationError as e:
        raise InvalidTestConfigException(e.errors())

    return test_suite
