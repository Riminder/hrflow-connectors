import os
from pathlib import Path
from unittest import mock

import pytest

from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.core.connector import Event, Reason, Status
from hrflow_connectors.core.tests import (
    ENVIRON_SECRETS_PREFIX,
    ConnectorTestConfig,
    InvalidJSONException,
    InvalidTestConfigException,
    InvalidYAMLException,
    NoTestConfigException,
    collect_connector_tests,
)
from tests.core.localusers.warehouse import UsersWarehouse
from tests.core.smartleads.warehouse import LeadsWarehouse

SmartLeads = Connector(
    name="SmartLeads",
    description="Test Connector for seamless users to leads integration",
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name="first_action",
            trigger_type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name="second_action",
            trigger_type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)
LocalUsers = Connector(
    name="LocalUsers",
    description="Local users connector",
    url="https://www.localusers.test/",
    actions=[],
)


@pytest.fixture
def connectors_directory():
    path = Path(__file__).parent
    yield path


@pytest.fixture
def global_secrets_file(connectors_directory):
    secrets_file = connectors_directory / "secrets.json"
    yield secrets_file
    secrets_file.unlink(missing_ok=True)


@pytest.fixture
def smartleads_test_config(connectors_directory):
    test_config = (
        connectors_directory / SmartLeads.model.name.lower() / "test-config.yaml"
    )
    yield test_config
    test_config.unlink(missing_ok=True)


@pytest.fixture
def smartleads_secrets_file(connectors_directory):
    secrets_file = connectors_directory / SmartLeads.model.name.lower() / "secrets.json"
    yield secrets_file
    secrets_file.unlink(missing_ok=True)


@pytest.fixture
def localusers_test_config(connectors_directory):
    test_config = (
        connectors_directory / LocalUsers.model.name.lower() / "test-config.yaml"
    )
    yield test_config
    test_config.unlink(missing_ok=True)


@pytest.fixture
def localusers_secrets_file(connectors_directory):
    secrets_file = connectors_directory / LocalUsers.model.name.lower() / "secrets.json"
    yield secrets_file
    secrets_file.unlink(missing_ok=True)


def test_schema():
    ConnectorTestConfig.schema()


def test_no_test_config(smartleads_test_config, connectors_directory):
    assert smartleads_test_config.exists() is False
    with pytest.raises(NoTestConfigException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_bad_yaml_test_config(smartleads_test_config, connectors_directory):
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes(
        """
actions:
  XXXXX
  first_action:
    - id: first_test
      origin_parameters:
      target_parameters:
        campain_id: my_camp
      status: success

        """.encode()
    )
    with pytest.raises(InvalidYAMLException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_invalid_action_test_config(smartleads_test_config, connectors_directory):
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes(
        """
# Missing mandatory origin_parameters
actions:
  first_action:
    - id: first_test
      target_parameters: {}
      status: success

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "first_action", 0, "origin_parameters")
    assert errors[0]["type"] == "value_error.missing"

    smartleads_test_config.write_bytes(
        """
# Missing target_parameters
actions:
  first_action:
    - id: first_test
      origin_parameters: {}
      status: success

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "first_action", 0, "target_parameters")
    assert errors[0]["type"] == "value_error.missing"

    smartleads_test_config.write_bytes(
        """
# Invalid status
actions:
  first_action:
    - id: first_test
      origin_parameters: {}
      target_parameters: {}
      status: not_a_valid_action_status

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "first_action", 0, "status")
    assert errors[0]["type"] == "type_error.enum"

    smartleads_test_config.write_bytes(
        """
# Invalid reason
actions:
  first_action:
    - id: first_test
      origin_parameters: {}
      target_parameters: {}
      reason: not_a_valid_reason

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "first_action", 0, "reason")
    assert errors[0]["type"] == "type_error.enum"

    smartleads_test_config.write_bytes(
        """
# Invalid events
actions:
  first_action:
    - id: first_test
      origin_parameters: {}
      target_parameters: {}
      events:
        not_a_valid_action_event: 12

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "first_action", 0, "events", "__key__")
    assert errors[0]["type"] == "type_error.enum"

    smartleads_test_config.write_bytes(
        """
# Action does not exist
actions:
  invalid_action_name:
    - id: first_test
      origin_parameters: {}
      target_parameters: {}
      status: success

        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("actions", "__key__")
    assert errors[0]["msg"].startswith(
        "No action 'invalid_action_name' found for connector {}".format(
            SmartLeads.model.name
        )
    )


def test_invalid_warehouse_test_config(localusers_test_config, connectors_directory):
    assert localusers_test_config.exists() is False
    localusers_test_config.write_bytes(
        """
# Missing mandatory parameters
warehouse:
  UsersWarehouse:
    read:
      - id: valid_parameters
        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=LocalUsers,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("warehouse", "UsersWarehouse", "read", 0, "parameters")
    assert errors[0]["type"] == "value_error.missing"

    localusers_test_config.write_bytes(
        """
# Warehouse doesnt not exist
warehouse:
  WarehouseDoesNotExist:
    read:
      - parameters: {}
        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=LocalUsers,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 1
    assert errors[0]["loc"] == ("warehouse", "__key__")
    assert errors[0]["msg"].startswith(
        "Warehouse 'WarehouseDoesNotExist' not found for connector {}".format(
            LocalUsers.model.name
        )
    )


def test_valid_action_config_no_secrets(smartleads_test_config, connectors_directory):
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes(
        """
actions:
  first_action:
    - id: first_test
      origin_parameters: {}
      target_parameters: {}
    - id: second_test
      origin_parameters: {}
      target_parameters: {}
      status: success
      reason: ""
    - origin_parameters: {}
      target_parameters:
        campain_id: my_camp
      status: fatal
      reason: bad_action_parameters
      events:
        read_success: 4
        read_failure: 1
        write_failure: 3
  second_action:
    - origin_parameters:
        gender: male
      target_parameters:
        campain_id: my_second_camp
      status: success
      events:
        read_success: 10
        read_failure: 0
        write_failure: 0
        """.encode()
    )
    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )
    assert test_suite.warehouse == dict()
    assert len(test_suite.actions) == 2
    first_action_tests = test_suite.actions["first_action"]
    second_action_tests = test_suite.actions["second_action"]

    assert len(first_action_tests) == 3

    assert first_action_tests[0].id == "first_test"
    assert first_action_tests[0].origin_parameters == dict()
    assert first_action_tests[0].target_parameters == dict()
    assert first_action_tests[0].status is None

    assert first_action_tests[1].id == "second_test"
    assert first_action_tests[1].origin_parameters == dict()
    assert first_action_tests[1].target_parameters == dict()
    assert first_action_tests[1].status is Status.success
    assert first_action_tests[1].reason is Reason.none

    assert first_action_tests[2].id is None
    assert first_action_tests[2].origin_parameters == dict()
    assert first_action_tests[2].target_parameters == dict(campain_id="my_camp")
    assert first_action_tests[2].status is Status.fatal
    assert first_action_tests[2].reason is Reason.bad_action_parameters
    assert first_action_tests[2].events == {
        Event.read_success: 4,
        Event.read_failure: 1,
        Event.write_failure: 3,
    }

    assert len(second_action_tests) == 1

    assert second_action_tests[0].id is None
    assert second_action_tests[0].origin_parameters == dict(gender="male")
    assert second_action_tests[0].target_parameters == dict(campain_id="my_second_camp")
    assert second_action_tests[0].status is Status.success
    assert second_action_tests[0].reason is None
    assert second_action_tests[0].events == {
        Event.read_success: 10,
        Event.read_failure: 0,
        Event.write_failure: 0,
    }


def test_valid_warehouse_config_no_secrets(
    localusers_test_config, connectors_directory
):
    assert localusers_test_config.exists() is False
    localusers_test_config.write_bytes(
        """
warehouse:
  UsersWarehouse:
    read:
      - id: empty_parameters
        parameters: {}
      - parameters:
          gender: male
      - parameters:
          gender: female
        expected_number_of_items: 5
  BadUsersWarehouse:
    read:
      - id: empty_parameters
        parameters: {}
        """.encode()
    )
    test_suite = collect_connector_tests(
        connector=LocalUsers,
        connectors_directory=connectors_directory,
    )
    assert len(test_suite.actions) == 0
    assert len(list(test_suite.warehouse.keys())) == 2
    users_warehouse_tests = test_suite.warehouse["UsersWarehouse"]
    bad_users_warehouse_tests = test_suite.warehouse["BadUsersWarehouse"]

    assert len(users_warehouse_tests.read) == 3

    assert users_warehouse_tests.read[0].id == "empty_parameters"
    assert users_warehouse_tests.read[0].parameters == dict()
    assert users_warehouse_tests.read[0].expected_number_of_items is None

    assert users_warehouse_tests.read[1].id is None
    assert users_warehouse_tests.read[1].parameters == dict(gender="male")
    assert users_warehouse_tests.read[1].expected_number_of_items is None

    assert users_warehouse_tests.read[2].id is None
    assert users_warehouse_tests.read[2].parameters == dict(gender="female")
    assert users_warehouse_tests.read[2].expected_number_of_items == 5

    assert len(bad_users_warehouse_tests.read) == 1

    assert bad_users_warehouse_tests.read[0].id == "empty_parameters"
    assert bad_users_warehouse_tests.read[0].parameters == dict()
    assert bad_users_warehouse_tests.read[0].expected_number_of_items is None


def test_failure_secret_not_found(
    global_secrets_file,
    smartleads_test_config,
    smartleads_secrets_file,
    connectors_directory,
):
    assert global_secrets_file.exists() is False
    assert smartleads_test_config.exists() is False
    assert smartleads_secrets_file.exists() is False
    smartleads_test_config.write_bytes(
        """
warehouse:
  LeadsWarehouse:
    read:
      - parameters:
          my_secret: $__SECRET_KEY
actions:
  first_action:
    - id: first_test
      origin_parameters:
        my_secret: $__SECRET_TOKEN
      target_parameters: {}
        """.encode()
    )
    with pytest.raises(InvalidTestConfigException) as excinfo:
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    errors = excinfo.value.args[0]
    assert len(errors) == 2
    assert errors[0]["loc"] == (
        "warehouse",
        "LeadsWarehouse",
        "read",
        0,
        "parameters",
        "my_secret",
    )
    assert errors[0][
        "msg"
    ] == "SECRET_KEY not found in secrets for connector {}".format(
        SmartLeads.model.name
    )
    assert errors[1]["loc"] == (
        "actions",
        "first_action",
        0,
        "origin_parameters",
        "my_secret",
    )
    assert errors[1][
        "msg"
    ] == "SECRET_TOKEN not found in secrets for connector {}".format(
        SmartLeads.model.name
    )


def test_failure_invalid_json_secrets(
    global_secrets_file,
    smartleads_test_config,
    smartleads_secrets_file,
    connectors_directory,
):
    assert global_secrets_file.exists() is False
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes(
        """
actions:
  first_action:
    - id: first_test
      origin_parameters:
        my_secret: $__SECRET_TOKEN
      target_parameters: {}
        """.encode()
    )

    # Global Secrets File
    global_secrets_file.write_bytes(
        """
not_at_valid_json_file
        """.encode()
    )
    assert smartleads_secrets_file.exists() is False
    with pytest.raises(InvalidJSONException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    global_secrets_file.unlink()

    smartleads_secrets_file.write_bytes(
        """
not_at_valid_json_file
        """.encode()
    )
    assert global_secrets_file.exists() is False
    with pytest.raises(InvalidJSONException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_secret_from_global_secrets_file(
    global_secrets_file,
    smartleads_test_config,
    smartleads_secrets_file,
    connectors_directory,
):
    assert smartleads_test_config.exists() is False
    assert global_secrets_file.exists() is False
    assert smartleads_secrets_file.exists() is False
    smartleads_test_config.write_bytes(
        """
warehouse:
  LeadsWarehouse:
    read:
      - parameters:
          my_secret: $__SECRET_KEY
actions:
  first_action:
    - id: first_test
      origin_parameters:
        my_secret: $__SECRET_TOKEN
      target_parameters: {}
        """.encode()
    )
    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.model.name.upper()
    )
    global_secrets_file.write_bytes(
        """
{{"{prefix}SECRET_TOKEN": "xxxToken", "{prefix}SECRET_KEY": "xxxKey"}}
        """.format(
            prefix=secrets_prefix
        ).encode()
    )
    assert smartleads_secrets_file.exists() is False
    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )
    assert (
        test_suite.actions["first_action"][0].origin_parameters["my_secret"]
        == "xxxToken"
    )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKey"
    )


def test_secret_from_connector_secrets_file(
    global_secrets_file,
    smartleads_test_config,
    smartleads_secrets_file,
    connectors_directory,
):
    assert smartleads_test_config.exists() is False
    assert global_secrets_file.exists() is False
    assert smartleads_secrets_file.exists() is False
    smartleads_test_config.write_bytes(
        """
warehouse:
  LeadsWarehouse:
    read:
      - parameters:
          my_secret: $__SECRET_KEY
actions:
  first_action:
    - id: first_test
      origin_parameters:
        my_secret: $__SECRET_TOKEN
      target_parameters: {}
        """.encode()
    )
    smartleads_secrets_file.write_bytes(
        """
{"SECRET_TOKEN": "xxxToken", "SECRET_KEY": "xxxKey"}
        """.encode()
    )
    assert global_secrets_file.exists() is False
    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )
    assert (
        test_suite.actions["first_action"][0].origin_parameters["my_secret"]
        == "xxxToken"
    )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKey"
    )


def test_secret_from_environment(
    global_secrets_file,
    smartleads_secrets_file,
    smartleads_test_config,
    connectors_directory,
):
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes(
        """
warehouse:
  LeadsWarehouse:
    read:
      - parameters:
          my_secret: $__SECRET_KEY
actions:
  first_action:
    - id: first_test
      origin_parameters:
        my_secret: $__SECRET_TOKEN
      target_parameters: {}
        """.encode()
    )
    assert smartleads_secrets_file.exists() is False
    assert global_secrets_file.exists() is False

    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.model.name.upper()
    )
    with mock.patch.dict(
        os.environ, {"{}SECRET_TOKEN".format(secrets_prefix): "xxxTokenFromEnv"}
    ):
        with mock.patch.dict(
            os.environ, {"{}SECRET_KEY".format(secrets_prefix): "xxxKeyFromEnv"}
        ):
            test_suite = collect_connector_tests(
                connector=SmartLeads,
                connectors_directory=connectors_directory,
            )
    assert (
        test_suite.actions["first_action"][0].origin_parameters["my_secret"]
        == "xxxTokenFromEnv"
    )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKeyFromEnv"
    )


def test_precedence_of_secrets(
    global_secrets_file,
    smartleads_secrets_file,
    smartleads_test_config,
    connectors_directory,
):
    assert smartleads_test_config.exists() is False
    assert smartleads_secrets_file.exists() is False
    assert global_secrets_file.exists() is False

    smartleads_test_config.write_bytes(
        """
warehouse:
  LeadsWarehouse:
    read:
      - parameters:
          my_secret: $__SECRET_KEY
        """.encode()
    )
    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.model.name.upper()
    )

    # Only global
    global_secrets_file.write_bytes(
        """
{{"{prefix}SECRET_KEY": "fromGlobal"}}
        """.format(
            prefix=secrets_prefix
        ).encode()
    )
    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "fromGlobal"
    )

    # Global and connector
    smartleads_secrets_file.write_bytes(
        """
{"SECRET_KEY": "fromConnector"}
        """.encode()
    )
    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "fromConnector"
    )

    # Global, connector and Environment
    with mock.patch.dict(
        os.environ, {"{}SECRET_KEY".format(secrets_prefix): "fromEnv"}
    ):
        test_suite = collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    assert (
        test_suite.warehouse["LeadsWarehouse"].read[0].parameters["my_secret"]
        == "fromEnv"
    )
