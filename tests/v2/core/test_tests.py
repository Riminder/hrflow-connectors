import os
import typing as t
from pathlib import Path
from unittest import mock

import pytest

from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Flow
from hrflow_connectors.v2.core.run import Event, Reason, Status
from hrflow_connectors.v2.core.tests import (
    ENVIRON_SECRETS_PREFIX,
    InvalidJSONException,
    InvalidTestConfigException,
    InvalidYAMLException,
    NoTestConfigException,
    collect_connector_tests,
)
from tests.v2.core.conftest import TypedSmartLeads


@pytest.fixture
def global_secrets_file(connectors_directory: Path) -> t.Iterator[Path]:
    secrets_file = connectors_directory / "secrets.json"
    yield secrets_file
    try:
        secrets_file.unlink()
    except FileNotFoundError:
        pass


@pytest.fixture
def smartleads_test_config(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
) -> t.Iterator[Path]:
    test_config = connectors_directory / SmartLeads.subtype / "test-config.yaml"
    yield test_config
    try:
        test_config.unlink()
    except FileNotFoundError:
        pass


@pytest.fixture
def smartleads_secrets_file(connectors_directory: Path, SmartLeads: TypedSmartLeads):
    secrets_file = connectors_directory / SmartLeads.subtype / "secrets.json"
    yield secrets_file
    try:
        secrets_file.unlink()
    except FileNotFoundError:
        pass


def test_no_test_config(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False
    with pytest.raises(NoTestConfigException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_bad_yaml_test_config(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False
    smartleads_test_config.write_bytes("""
actions:
  XXXXX
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
      hrflow_auth:
        api_key: xxx
        api_user: xxx
      status: success

        """.encode())
    with pytest.raises(InvalidYAMLException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_invalid_action_test_config_missing_mandatory_fields(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
# Missing mandatory connector_auth
actions:
  create_jobs_in_hrflow:
    - id: first_test
      hrflow_auth: {}
      status: success

        """.encode())

    with pytest.raises(
        InvalidTestConfigException, match="missing required field `connector_auth`"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )
    smartleads_test_config.write_bytes("""
# Missing mandatory hrflow_auth
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth: {}
      status: success

        """.encode())

    with pytest.raises(
        InvalidTestConfigException, match="missing required field `hrflow_auth`"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_invalid_action_test_config_invalid_values(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
# Invalid status
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth: {}
      hrflow_auth: {}
      status: not_a_valid_action_status

        """.encode())
    with pytest.raises(
        InvalidTestConfigException,
        match="Invalid enum value 'not_a_valid_action_status'",
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
# Invalid reason
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth: {}
      hrflow_auth: {}
      reason: not_a_valid_reason

        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="Invalid enum value 'not_a_valid_reason'"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
# Invalid events
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth: {}
      hrflow_auth: {}
      events:
        not_a_valid_action_event: 12

        """.encode())
    with pytest.raises(
        InvalidTestConfigException,
        match="'not_a_valid_action_event' is not a valid Event",
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


# FIXME remove the xfail decorator and
# **corresponding pragma: nocover**
# once issue has an answer
# https://github.com/jcrist/msgspec/issues/775
@pytest.mark.xfail
def test_invalid_action_test_config_invalid_action_name(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
# Action name is not valid
actions:
  invalid_action_name:
    - id: first_test
      connector_auth: {}
      hrflow_auth: {}
      status: success

        """.encode())
    with pytest.raises(
        InvalidTestConfigException,
        match="No action 'invalid_action_name' found for connector",
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    unavailable_flow = Flow(Mode.create, Entity.application, Direction.inbound)
    unavailable_action = unavailable_flow.name(SmartLeads.subtype)
    for flow in SmartLeads.flows:
        assert (
            flow.mode != unavailable_flow.mode
            or flow.entity != unavailable_flow.entity
            or flow.direction != unavailable_flow.direction
        )

    smartleads_test_config.write_bytes(f"""
# Action name is within connectors flows
actions:
  {unavailable_action}:
    - id: first_test
      connector_auth: {{}}
      hrflow_auth: {{}}
      status: success

        """.encode())
    with pytest.raises(
        InvalidTestConfigException,
        match=f"No action '{unavailable_action}' found for connector",
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_invalid_warehouse_test_config_missing_mandatory_fields(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
# Missing mandatory parameters
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: valid_parameters
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="missing required field `mode`"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
# Missing mandatory parameters
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: valid_parameters
        mode: create
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="missing required field `entity`"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
# Missing mandatory parameters
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: valid_parameters
        mode: create
        entity: job
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="missing required field `auth_parameters`"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_invalid_warehouse_test_config_invalid_values(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: valid_parameters
        mode: not_valid_mode
        entity: job
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="Invalid enum value 'not_valid_mode'"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: valid_parameters
        mode: create
        entity: spaceship
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="Invalid enum value 'spaceship'"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


# FIXME remove the xfail decorator and
# **corresponding pragma: nocover**
# once issue has an answer
# https://github.com/jcrist/msgspec/issues/775
@pytest.mark.xfail
def test_invalid_warehouse_test_config_bad_warehouse_name(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
# Missing mandatory parameters
warehouse:
  BadNameWarehouse:
    read:
      - id: valid_parameters
        mode: create
        entity: job
        """.encode())
    with pytest.raises(
        InvalidTestConfigException,
        match="Warehouse 'BadNameWarehouse' not found for connector SmartLeads",
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_valid_action_config_no_secrets(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth: {}
      hrflow_auth: {}
    - id: second_test
      connector_auth: {}
      hrflow_auth: {}
      status: success
      reason: none
    - connector_auth: {}
      hrflow_auth:
        campain_id: my_camp
      status: fatal
      reason: bad_origin_parameters
      events:
        read_success: 4
        read_failure: 1
        write_failure: 3
  archive_jobs_in_smartleads:
    - connector_auth:
        gender: male
      hrflow_auth:
        campain_id: my_second_camp
      status: success
      events:
        read_success: 10
        read_failure: 0
        write_failure: 0
        """.encode())

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert test_suite.warehouse == dict()
    assert len(test_suite.actions) == 2

    first_action_tests = test_suite.actions["create_jobs_in_hrflow"]
    second_action_tests = test_suite.actions["archive_jobs_in_smartleads"]

    assert len(first_action_tests) == 3

    assert first_action_tests[0].id == "first_test"
    assert first_action_tests[0].connector_auth == dict()
    assert first_action_tests[0].hrflow_auth == dict()
    assert first_action_tests[0].status is None

    assert first_action_tests[1].id == "second_test"
    assert first_action_tests[1].connector_auth == dict()
    assert first_action_tests[1].hrflow_auth == dict()
    assert first_action_tests[1].status is Status.success
    assert first_action_tests[1].reason is Reason.none

    assert first_action_tests[2].id is None
    assert first_action_tests[2].connector_auth == dict()
    assert first_action_tests[2].hrflow_auth == dict(campain_id="my_camp")
    assert first_action_tests[2].status is Status.fatal
    assert first_action_tests[2].reason is Reason.bad_origin_parameters
    assert first_action_tests[2].events == {
        Event.read_success: 4,
        Event.read_failure: 1,
        Event.write_failure: 3,
    }

    assert len(second_action_tests) == 1

    assert second_action_tests[0].id is None
    assert second_action_tests[0].connector_auth == dict(gender="male")
    assert second_action_tests[0].hrflow_auth == dict(campain_id="my_second_camp")
    assert second_action_tests[0].status is Status.success
    assert second_action_tests[0].reason is None
    assert second_action_tests[0].events == {
        Event.read_success: 10,
        Event.read_failure: 0,
        Event.write_failure: 0,
    }


def test_valid_warehouse_config_no_secrets(
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - id: empty_parameters
        mode: create
        entity: job
        auth_parameters:
          secret: __thesecret
        parameters: {}
        incremental: true
      - parameters:
          gender: male
        auth_parameters: {}
        mode: archive
        entity: profile
        incremental_token: __xId
        incremental: false
      - parameters:
          gender: female
        auth_parameters:
          secret: __thesecret
        mode: update
        entity: job
        expected_number_of_items: 5
        """.encode())

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert len(test_suite.actions) == 0
    assert len(list(test_suite.warehouse.keys())) == 1

    users_warehouse_tests = test_suite.warehouse["SmartLeadsWarehouse"]

    assert len(users_warehouse_tests.read) == 3

    assert users_warehouse_tests.read[0].id == "empty_parameters"
    assert users_warehouse_tests.read[0].mode is Mode.create
    assert users_warehouse_tests.read[0].entity is Entity.job
    assert users_warehouse_tests.read[0].auth_parameters == dict(secret="__thesecret")
    assert users_warehouse_tests.read[0].parameters == dict()
    assert users_warehouse_tests.read[0].incremental is True
    assert users_warehouse_tests.read[0].incremental_token is None
    assert users_warehouse_tests.read[0].expected_number_of_items is None

    assert users_warehouse_tests.read[1].id is None
    assert users_warehouse_tests.read[1].mode is Mode.archive
    assert users_warehouse_tests.read[1].entity is Entity.profile
    assert users_warehouse_tests.read[1].auth_parameters == dict()
    assert users_warehouse_tests.read[1].parameters == dict(gender="male")
    assert users_warehouse_tests.read[1].expected_number_of_items is None
    assert users_warehouse_tests.read[1].incremental is False
    assert users_warehouse_tests.read[1].incremental_token == "__xId"

    assert users_warehouse_tests.read[2].id is None
    assert users_warehouse_tests.read[2].mode is Mode.update
    assert users_warehouse_tests.read[2].entity is Entity.job
    assert users_warehouse_tests.read[2].auth_parameters == dict(secret="__thesecret")
    assert users_warehouse_tests.read[2].parameters == dict(gender="female")
    assert users_warehouse_tests.read[2].expected_number_of_items == 5
    assert users_warehouse_tests.read[2].incremental is False
    assert users_warehouse_tests.read[2].incremental_token is None


def test_failure_secret_not_found(
    global_secrets_file: Path,
    smartleads_test_config: Path,
    smartleads_secrets_file: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert global_secrets_file.exists() is False
    assert smartleads_test_config.exists() is False
    assert smartleads_secrets_file.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: create
        entity: job
        auth_parameters: {}
        parameters:
          my_secret: $__SECRET_KEY
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="'SECRET_KEY' not found in secrets"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: create
        entity: job
        auth_parameters: {}
        parameters:
          my_secret: fixed_this_time
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())
    with pytest.raises(
        InvalidTestConfigException, match="'SECRET_TOKEN' not found in secrets"
    ):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_failure_invalid_json_secrets(
    global_secrets_file: Path,
    smartleads_test_config: Path,
    smartleads_secrets_file: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert global_secrets_file.exists() is False
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())

    # Global Secrets File
    global_secrets_file.write_bytes("""
not_at_valid_json_file
        """.encode())

    assert smartleads_secrets_file.exists() is False

    with pytest.raises(InvalidJSONException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )

    global_secrets_file.unlink()

    smartleads_secrets_file.write_bytes("""
not_at_valid_json_file
        """.encode())

    assert global_secrets_file.exists() is False

    with pytest.raises(InvalidJSONException):
        collect_connector_tests(
            connector=SmartLeads,
            connectors_directory=connectors_directory,
        )


def test_secret_from_global_secrets_file(
    global_secrets_file: Path,
    smartleads_test_config: Path,
    smartleads_secrets_file: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False
    assert global_secrets_file.exists() is False
    assert smartleads_secrets_file.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: create
        entity: job
        auth_parameters: {}
        parameters:
          my_secret: $__SECRET_KEY
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())

    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.name.upper()
    )
    global_secrets_file.write_bytes("""
{{"{prefix}SECRET_TOKEN": "xxxToken", "{prefix}SECRET_KEY": "xxxKey"}}
        """.format(prefix=secrets_prefix).encode())

    assert smartleads_secrets_file.exists() is False

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert (
        test_suite.actions["create_jobs_in_hrflow"][0].connector_auth["my_secret"]
        == "xxxToken"
    )
    assert (
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKey"
    )


def test_secret_from_connector_secrets_file(
    global_secrets_file: Path,
    smartleads_test_config: Path,
    smartleads_secrets_file: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False
    assert global_secrets_file.exists() is False
    assert smartleads_secrets_file.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: archive
        entity: job
        auth_parameters: {}
        parameters:
          my_secret: $__SECRET_KEY
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())

    smartleads_secrets_file.write_bytes("""
{"SECRET_TOKEN": "xxxToken", "SECRET_KEY": "xxxKey"}
        """.encode())

    assert global_secrets_file.exists() is False

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert (
        test_suite.actions["create_jobs_in_hrflow"][0].connector_auth["my_secret"]
        == "xxxToken"
    )
    assert (
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKey"
    )


def test_secret_from_environment(
    global_secrets_file: Path,
    smartleads_secrets_file: Path,
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: archive
        entity: profile
        auth_parameters: {}
        parameters:
          my_secret: $__SECRET_KEY
actions:
  create_jobs_in_hrflow:
    - id: first_test
      connector_auth:
        my_secret: $__SECRET_TOKEN
      hrflow_auth: {}
        """.encode())

    assert smartleads_secrets_file.exists() is False
    assert global_secrets_file.exists() is False

    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.name.upper()
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
        test_suite.actions["create_jobs_in_hrflow"][0].connector_auth["my_secret"]
        == "xxxTokenFromEnv"
    )
    assert (
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
        == "xxxKeyFromEnv"
    )


def test_precedence_of_secrets(
    global_secrets_file: Path,
    smartleads_secrets_file: Path,
    smartleads_test_config: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    assert smartleads_test_config.exists() is False
    assert smartleads_secrets_file.exists() is False
    assert global_secrets_file.exists() is False

    smartleads_test_config.write_bytes("""
warehouse:
  SmartLeadsWarehouse:
    read:
      - mode: archive
        entity: profile
        auth_parameters: {}
        parameters:
          my_secret: $__SECRET_KEY
        """.encode())
    secrets_prefix = ENVIRON_SECRETS_PREFIX.format(
        connector_name=SmartLeads.name.upper()
    )

    # Only global
    global_secrets_file.write_bytes("""
{{"{prefix}SECRET_KEY": "fromGlobal"}}
        """.format(prefix=secrets_prefix).encode())

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert (
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
        == "fromGlobal"
    )

    # Global and connector
    smartleads_secrets_file.write_bytes("""
{"SECRET_KEY": "fromConnector"}
        """.encode())

    test_suite = collect_connector_tests(
        connector=SmartLeads,
        connectors_directory=connectors_directory,
    )

    assert (
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
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
        test_suite.warehouse["SmartLeadsWarehouse"].read[0].parameters["my_secret"]
        == "fromEnv"
    )
