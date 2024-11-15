import logging
import typing as t
from collections import namedtuple
from importlib import import_module

import pytest

import hrflow_connectors.v1 as v1
from hrflow_connectors.v1.core.tests import collect_connector_tests

WarehouseReadTest = namedtuple(
    "WarehouseReadTest",
    "read, parameters, read_mode, read_from, expected_number_of_items",
)


def parameterize_read_warehouse_tests(
    connectors: t.List[str],
) -> t.List[pytest.param]:
    params = []
    for connector in connectors:
        try:
            connector = getattr(v1, connector)
        except AttributeError:
            raise Exception(
                "Coundn't find connector '{}' : Must be one of {}. Skipping...".format(
                    connector,
                    [connector.model.name for connector in v1.__CONNECTORS__],
                ),
            )
        connector_test_suite = collect_connector_tests(connector)
        if connector_test_suite.warehouse:
            warehouses = import_module(
                "hrflow_connectors.connectors.v1.{}.warehouse".format(
                    connector.model.subtype
                )
            )
            for (
                warehouse_name,
                warehouse_tests,
            ) in connector_test_suite.warehouse.items():
                warehouse = getattr(warehouses, warehouse_name)
                for i, warehouse_test in enumerate(warehouse_tests.read):
                    params.append(
                        pytest.param(
                            WarehouseReadTest(
                                read=warehouse.read,
                                parameters=warehouse_test.parameters,
                                read_mode=warehouse_test.read_mode,
                                read_from=warehouse_test.read_from,
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


def test_read_warehouse(warehouse_read_test_params_v1):
    logger = logging.getLogger("warehouse_read_test_v1")
    adapter = logging.LoggerAdapter(logger, extra=dict())

    items = list(
        warehouse_read_test_params_v1.read(
            adapter,
            warehouse_read_test_params_v1.read.parameters(
                **warehouse_read_test_params_v1.parameters
            ),
            warehouse_read_test_params_v1.read_mode,
            warehouse_read_test_params_v1.read_from,
        )
    )
    if warehouse_read_test_params_v1.expected_number_of_items is not None:
        assert len(items) == warehouse_read_test_params_v1.expected_number_of_items
    else:
        assert len(items) > 0
