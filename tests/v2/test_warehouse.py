import logging
import typing as t
from dataclasses import dataclass
from importlib import import_module

import pytest

import hrflow_connectors.v2 as v2
from hrflow_connectors.v2.core.connector import Connector
from hrflow_connectors.v2.core.msgspec_pydantic_compat import serialize
from hrflow_connectors.v2.core.tests import ReadTest, collect_connector_tests
from hrflow_connectors.v2.core.warehouse import Warehouse


@dataclass
class ReadTestParam:
    warehouse: Warehouse
    config: ReadTest


def parameterize_read_warehouse_tests(
    connectors: list[str],
) -> list[ReadTestParam]:
    params = []
    for connector in connectors:
        try:
            connector = t.cast(Connector, getattr(v2, connector))
        except AttributeError:
            raise Exception(
                "Coundn't find connector '{}' : Must be one of {}. Skipping...".format(
                    connector,
                    [connector.name for connector in v2.__CONNECTORS__],
                ),
            )
        connector_test_suite = collect_connector_tests(connector)
        if connector_test_suite.warehouse:
            warehouses = import_module(
                "hrflow_connectors.connectors.v2.{}.warehouse".format(connector.subtype)
            )
            for (
                warehouse_name,
                warehouse_tests,
            ) in connector_test_suite.warehouse.items():
                warehouse = getattr(warehouses, warehouse_name)
                for i, warehouse_test in enumerate(warehouse_tests.read):
                    params.append(
                        pytest.param(
                            ReadTestParam(
                                warehouse=warehouse,
                                config=warehouse_test,
                            ),
                            id="{}_warehouse:{}_{}".format(
                                connector.subtype,
                                warehouse_name,
                                warehouse_test.id or i,
                            ),
                        )
                    )
    return params


def test_read_warehouse(warehouse_read_test_params_v2: ReadTestParam):
    logger = logging.getLogger("warehouse_read_test_v2")
    adapter = logging.LoggerAdapter(logger, extra=dict())

    config = warehouse_read_test_params_v2.config
    warehouse = warehouse_read_test_params_v2.warehouse

    aisle = warehouse.get_aisle(config.entity)

    assert aisle is not None, f"{config.entity} is not supported by warehouse"
    assert aisle.read is not None, f"Aisle for {config.entity} is not readable"

    parameters_schema = aisle.parameters("read", config.mode)
    assert parameters_schema is not None, f"Aisle for {config.entity} is not readable"

    items = list(
        aisle.read(
            mode=config.mode,
            adapter=adapter,
            auth_parameters=serialize(config.auth_parameters, warehouse.auth),
            parameters=serialize(config.parameters, parameters_schema),
            incremental=config.incremental,
            incremental_token=config.incremental_token,
        )
    )
    if config.expected_number_of_items is not None:
        assert len(items) == config.expected_number_of_items
    else:
        assert len(items) > 0
