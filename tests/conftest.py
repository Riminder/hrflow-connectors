import random
import string
from pathlib import Path

import pytest

from hrflow_connectors import __CONNECTORS__
from tests.v1.test_connector import (
    parameterize_connector_action_tests as parameterize_connector_action_tests_v1,
)
from tests.v1.test_warehouse import (
    parameterize_read_warehouse_tests as parameterize_read_warehouse_tests_v1,
)


def pytest_addoption(parser):
    parser.addoption(
        "--connector-v1",
        action="append",
        default=[],
        help="list of v1 connectors for which to run integration tests",
    )
    parser.addoption(
        "--allconnectors-v1",
        action="store_true",
        default=False,
        help="Run integration tests for all v1 connectors",
    )


def pytest_generate_tests(metafunc):
    if "connector_action_test_params_v1" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v1") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__]
        else:
            connectors = metafunc.config.getoption("connector_v1")
        params = parameterize_connector_action_tests_v1(connectors=connectors)
        metafunc.parametrize("connector_action_test_params_v1", params)

    if "warehouse_read_test_params_v1" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v1") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__]
        else:
            connectors = metafunc.config.getoption("connector_v1")
        params = parameterize_read_warehouse_tests_v1(connectors=connectors)
        metafunc.parametrize("warehouse_read_test_params_v1", params)


def random_workflow_id() -> str:
    return "".join([random.choice(string.ascii_letters) for _ in range(10)])


@pytest.fixture
def test_connectors_directory_v1():
    return (
        Path(__file__).parent
        / "v1"
        / "core"
        / "src"
        / "hrflow_connectors"
        / "connectors"
    )
