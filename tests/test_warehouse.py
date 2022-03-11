import logging
import typing as t
from collections import namedtuple
from importlib import import_module

import pytest

import hrflow_connectors
from hrflow_connectors.core.tests import collect_connector_tests

WarehousePullTest = namedtuple(
    "WarehousePullTest",
    "pull, parameters, expected_number_of_items",
)


def parameterize_pull_warehouse_tests(
    connectors: t.List[str],
) -> t.List[pytest.param]:
    params = []
    for connector in connectors:
        try:
            connector = getattr(hrflow_connectors, connector)
        except AttributeError:
            raise Exception(
                "Coundn't find connector '{}' : Must be one of {}. Skipping...".format(
                    connector,
                    [
                        connector.model.name
                        for connector in hrflow_connectors.__CONNECTORS__
                    ],
                ),
            )
        connector_test_suite = collect_connector_tests(connector)
        if connector_test_suite.warehouse:
            warehouses = import_module(
                "hrflow_connectors.connectors.{}.warehouse".format(
                    connector.model.name.lower()
                )
            )
            for (
                warehouse_name,
                warehouse_tests,
            ) in connector_test_suite.warehouse.items():
                warehouse = getattr(warehouses, warehouse_name)
                for i, warehouse_test in enumerate(warehouse_tests.pull):
                    params.append(
                        pytest.param(
                            WarehousePullTest(
                                pull=warehouse.pull,
                                parameters=warehouse_test.parameters,
                                expected_number_of_items=warehouse_test.expected_number_of_items,  # noqa
                            ),
                            id="{}_warehouse:{}_{}".format(
                                connector.model.name,
                                warehouse_name,
                                warehouse_test.id or i,
                            ),
                        )
                    )
    return params


def test_pull_warehouse(warehouse_pull_test_params):
    logger = logging.getLogger("warehouse_pull_test")
    adapter = logging.LoggerAdapter(logger, extra=dict())

    items = list(
        warehouse_pull_test_params.pull(
            adapter,
            warehouse_pull_test_params.pull.parameters(
                **warehouse_pull_test_params.parameters
            ),
        )
    )
    if warehouse_pull_test_params.expected_number_of_items is not None:
        assert len(items) == warehouse_pull_test_params.expected_number_of_items
