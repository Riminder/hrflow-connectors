import random
import string
from pathlib import Path

import pytest

from hrflow_connectors import __CONNECTORS__ as __CONNECTORS__V1
from hrflow_connectors.v2 import __CONNECTORS__ as __CONNECTORS__V2
from tests.v1.test_connector import (
    parameterize_connector_action_tests as parameterize_connector_action_tests_v1,
)
from tests.v1.test_warehouse import (
    parameterize_read_warehouse_tests as parameterize_read_warehouse_tests_v1,
)
from tests.v2.test_connector import (
    parameterize_connector_action_tests as parameterize_connector_action_tests_v2,
)
from tests.v2.test_warehouse import (
    parameterize_read_warehouse_tests as parameterize_read_warehouse_tests_v2,
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
    parser.addoption(
        "--connector-v2",
        action="append",
        default=[],
        help="list of v2 connectors for which to run integration tests",
    )
    parser.addoption(
        "--allconnectors-v2",
        action="store_true",
        default=False,
        help="Run integration tests for all v2 connectors",
    )


def pytest_generate_tests(metafunc):
    if "connector_action_test_params_v1" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v1") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__V1]
        else:
            connectors = metafunc.config.getoption("connector_v1")
        params = parameterize_connector_action_tests_v1(connectors=connectors)
        metafunc.parametrize("connector_action_test_params_v1", params)

    if "warehouse_read_test_params_v1" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v1") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__V1]
        else:
            connectors = metafunc.config.getoption("connector_v1")
        params = parameterize_read_warehouse_tests_v1(connectors=connectors)
        metafunc.parametrize("warehouse_read_test_params_v1", params)

    if "connector_action_test_params_v2" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v2") is True:
            connectors = [connector.name for connector in __CONNECTORS__V2]
        else:
            connectors = metafunc.config.getoption("connector_v2")
        params = parameterize_connector_action_tests_v2(connectors=connectors)
        metafunc.parametrize("connector_action_test_params_v2", params)

    if "warehouse_read_test_params_v2" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors_v2") is True:
            connectors = [connector.name for connector in __CONNECTORS__V2]
        else:
            connectors = metafunc.config.getoption("connector_v2")
        params = parameterize_read_warehouse_tests_v2(connectors=connectors)
        metafunc.parametrize("warehouse_read_test_params_v2", params)


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
