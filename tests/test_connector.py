import typing as t
from collections import namedtuple

import pytest

import hrflow_connectors
from hrflow_connectors.core.tests import collect_connector_tests

ConnectorActionTestParams = namedtuple(
    "ConnectorActionTestParams",
    "action, origin_parameters, target_parameters, expected_status,"
    " expected_reason, expected_events, workflow_id",
)


def parameterize_connector_action_tests(
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
        for i, (action_name, action_tests) in enumerate(
            connector_test_suite.actions.items()
        ):
            action = getattr(connector, action_name)
            for action_test in action_tests:
                params.append(
                    pytest.param(
                        ConnectorActionTestParams(
                            action=action,
                            workflow_id="pytest_{}_{}".format(
                                connector.model.name, action_name
                            ),
                            origin_parameters=action_test.origin_parameters,
                            target_parameters=action_test.target_parameters,
                            expected_status=action_test.status,
                            expected_reason=action_test.reason,
                            expected_events=action_test.events,
                        ),
                        id="{}_action:{}_{}".format(
                            connector.model.name, action_name, action_test.id or i
                        ),
                    )
                )
    return params


def test_connector_action(connector_action_test_params):
    result = connector_action_test_params.action(
        workflow_id=connector_action_test_params.workflow_id,
        action_parameters=dict(),
        origin_parameters=connector_action_test_params.origin_parameters,
        target_parameters=connector_action_test_params.target_parameters,
    )
    if connector_action_test_params.expected_status is not None:
        assert result.status == connector_action_test_params.expected_status
    if connector_action_test_params.expected_reason is not None:
        assert result.reason == connector_action_test_params.expected_reason
    if connector_action_test_params.expected_events is not None:
        for event in connector_action_test_params.expected_events:
            assert (
                result.events[event]
                == connector_action_test_params.expected_events[event]
            )
