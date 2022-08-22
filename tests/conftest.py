import random
import string

from hrflow_connectors import __CONNECTORS__
from tests.test_connector import parameterize_connector_action_tests
from tests.test_warehouse import parameterize_read_warehouse_tests


def pytest_addoption(parser):
    parser.addoption(
        "--connector",
        action="append",
        default=[],
        help="list of connectors for which to run integration tests",
    )
    parser.addoption(
        "--allconnectors",
        action="store_true",
        default=False,
        help="Run integration tests for all connectors",
    )


def pytest_generate_tests(metafunc):
    if "connector_action_test_params" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__]
        else:
            connectors = metafunc.config.getoption("connector")
        params = parameterize_connector_action_tests(connectors=connectors)
        metafunc.parametrize("connector_action_test_params", params)

    if "warehouse_read_test_params" in metafunc.fixturenames:
        if metafunc.config.getoption("allconnectors") is True:
            connectors = [connector.model.name for connector in __CONNECTORS__]
        else:
            connectors = metafunc.config.getoption("connector")
        params = parameterize_read_warehouse_tests(connectors=connectors)
        metafunc.parametrize("warehouse_read_test_params", params)


def random_workflow_id() -> str:
    return "".join([random.choice(string.ascii_letters) for _ in range(10)])
