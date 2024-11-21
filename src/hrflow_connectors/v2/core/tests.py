import json
import os
import typing as t
from abc import abstractmethod
from collections import Counter
from contextlib import contextmanager
from contextvars import ContextVar
from enum import Enum
from importlib import import_module
from pathlib import Path

import yaml
from msgspec import Struct, ValidationError, convert, field

from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.connector import Connector
from hrflow_connectors.v2.core.run import Event, Reason, Status
from hrflow_connectors.v2.core.utils import CONNECTORS_DIRECTORY
from hrflow_connectors.v2.core.warehouse import Warehouse

PROJECT_DIRECTORY = Path(__file__).parent.parent.parent.parent.parent

SECRETS_PREFIX = "$__"
ENVIRON_SECRETS_PREFIX = "HRFLOW_CONNECTORS_V2_{connector_name}_"
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
def secrets(connector_name: str, connector_subtype: str, connectors_directory: Path):
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

    connector_secrets_file = connectors_directory / connector_subtype / "secrets.json"
    if connector_secrets_file.exists():
        try:
            connector_secrets = json.loads(connector_secrets_file.read_text())
        except json.JSONDecodeError as e:
            raise InvalidJSONException(
                "Failed to JSON decode secrets file for connector {} with error {}"
                .format(connector_name, e)
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
    connector_name_token = ConnectorName.set(connector.name)
    actions_token = ActionNames.set(
        [flow.name(connector.subtype) for flow in connector.flows]
    )
    yield
    ConnectorName.reset(connector_name_token)
    ActionNames.reset(actions_token)


@contextmanager
def warehouses(connector_subtype: str, connectors_directory: Path):
    if connectors_directory is CONNECTORS_DIRECTORY:  # pragma: no cover
        warehouse_module = import_module(
            "hrflow_connectors.v2.connectors.{}.warehouse".format(connector_subtype)
        )
    else:
        import_from = connectors_directory.relative_to(PROJECT_DIRECTORY)
        warehouse_module = import_module(
            "{}.{}.warehouse".format(
                str(import_from).replace("/", "."), connector_subtype
            )
        )
    warehouse_names = []
    for attribute, value in warehouse_module.__dict__.items():
        if isinstance(value, Warehouse):
            warehouse_names.append(attribute)
    token = WarehouseNames.set(warehouse_names)
    yield
    WarehouseNames.reset(token)


class Validated:
    @classmethod
    @abstractmethod
    def validate(cls, value: t.Any) -> t.Any:
        raise NotImplementedError()  # pragma: nocover


# This was added to avoid failure during
# msgspec convert. What happens is that
# msgspec is expecting instances of ParameterValue
# but ParameterValue is more a validation wrapper
# with no type as if depends on what might be present
# in secrets
class NeverFailIsInstanceMeta(type):
    def __instancecheck__(cls, inst):
        return True


class ParameterValue(Validated, metaclass=NeverFailIsInstanceMeta):
    @classmethod
    def validate(cls, value: t.Any):
        if isinstance(value, str) and value.startswith(SECRETS_PREFIX):
            key = value.replace(SECRETS_PREFIX, "", 1)
            if key not in Secrets.get(dict()):
                raise TypeError(
                    "'{}' not found in secrets for connector {}".format(
                        key, ConnectorName.get("")
                    )
                )
            return Secrets.get()[key]
        return value


class ActionName(Validated):
    @classmethod
    def validate(cls, value: t.Any):  # pragma: nocover
        action_names = ActionNames.get([])
        if value not in action_names:
            raise TypeError(
                "No action '{}' found for connector {}. Should be one of {}".format(
                    value, ConnectorName.get(""), action_names
                )
            )
        return value


class WarehouseName(Validated):
    @classmethod
    def validate(cls, value: t.Any):  # pragma: nocover
        warehoue_names = WarehouseNames.get([])
        if value not in warehoue_names:
            raise TypeError(
                "Warehouse '{}' not found for connector {}. Should be one of {}".format(
                    value, ConnectorName.get(""), warehoue_names
                )
            )
        return value


class ReadTest(Struct):
    mode: Mode
    entity: Entity
    auth_parameters: dict[str, ParameterValue]
    id: t.Optional[str] = None
    parameters: dict[str, ParameterValue] = field(default_factory=dict)
    incremental: bool = False
    incremental_token: t.Optional[str] = None
    expected_number_of_items: t.Optional[int] = None


class WarehouseTests(Struct):
    read: list[ReadTest] = field(default_factory=list)


class ActionTest(Struct):
    connector_auth: dict[str, ParameterValue]
    hrflow_auth: dict[str, ParameterValue]
    id: t.Optional[str] = None
    pull_parameters: dict[str, ParameterValue] = field(default_factory=dict)
    push_parameters: dict[str, ParameterValue] = field(default_factory=dict)
    status: t.Optional[Status] = None
    reason: t.Optional[Reason] = None
    events: t.Optional[t.Counter[Event]] = None


class ConnectorTestConfig(Struct):
    if t.TYPE_CHECKING:
        warehouse: dict[str, WarehouseTests] = field(
            default_factory=dict
        )  # pragma: nocover
        actions: dict[str, list[ActionTest]] = field(
            default_factory=dict
        )  # pragma: nocover
    else:
        warehouse: dict[WarehouseName, WarehouseTests] = field(default_factory=dict)
        actions: dict[ActionName, list[ActionTest]] = field(default_factory=dict)


def dec_hook(type: type, obj: t.Any) -> t.Any:
    if (
        t.get_origin(type) is Counter
        and len(t.get_args(type)) == 1
        and issubclass((EnumModel := t.get_args(type)[0]), Enum)
    ):
        return Counter({EnumModel(key): value for key, value in obj.items()})

    if issubclass(type, Validated):
        return type.validate(obj)

    else:
        raise NotImplementedError(
            f"Objects of type {type} are not supported"
        )  # pragma: nocover


def collect_connector_tests(
    connector: Connector, connectors_directory: Path = CONNECTORS_DIRECTORY
):
    connector_name = connector.name
    connector_subtype = connector.subtype
    test_config_file = connectors_directory / connector_subtype / "test-config.yaml"
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
            connector_subtype=connector_subtype,
            connectors_directory=connectors_directory,
        ):
            with actions(
                connector=connector,
            ):
                with secrets(
                    connector_name=connector_name,
                    connector_subtype=connector_subtype,
                    connectors_directory=connectors_directory,
                ):
                    test_suite = convert(
                        test_config, ConnectorTestConfig, dec_hook=dec_hook
                    )
    except ValidationError as e:
        raise InvalidTestConfigException(e.args)

    return test_suite
