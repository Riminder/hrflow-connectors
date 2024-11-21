import typing as t
from dataclasses import dataclass

import pytest

import hrflow_connectors.v2 as v2
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface
from hrflow_connectors.v2.core.tests import ActionTest, collect_connector_tests


@dataclass
class ActionTestParam:
    workflow_id: str
    action: PublicActionInterface
    config: ActionTest


def parameterize_connector_action_tests(
    connectors: list[str],
) -> list[ActionTestParam]:
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
        for i, (action_name, action_tests) in enumerate(
            connector_test_suite.actions.items()
        ):
            action = getattr(connector, action_name)
            for action_test in action_tests:
                params.append(
                    pytest.param(
                        ActionTestParam(
                            action=action,
                            config=action_test,
                            workflow_id="pytest_{}_{}".format(
                                connector.name, action_name
                            ),
                        ),
                        id="{}_action:{}_{}".format(
                            connector.name, action_name, action_test.id or i
                        ),
                    )
                )
    return params


def test_connector_action(connector_action_test_params_v2: ActionTestParam):
    config = connector_action_test_params_v2.config

    result = connector_action_test_params_v2.action(
        workflow_id=connector_action_test_params_v2.workflow_id,
        connector_auth=config.connector_auth,
        hrflow_auth=config.hrflow_auth,
        pull_parameters=config.pull_parameters,
        push_parameters=config.push_parameters,
    )
    if config.status is not None:
        assert result.status == config.status
    if config.reason is not None:
        assert result.reason == config.reason
    if config.events is not None:
        for event in config.events:
            assert result.events[event] == config.events[event]
