import json
from collections import Counter
from pathlib import Path

import pytest
from pydantic import ValidationError

from hrflow_connectors import hrflow_connectors_manifest
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.core.connector import (
    ActionRunEvents,
    ActionRunResult,
    ActionStatus,
    FatalError,
)
from tests.core.localusers.warehouse import USERS_DB, BadUsersWarehouse, UsersWarehouse
from tests.core.smartleads.warehouse import LEADS_DB, BadLeadsWarehouse, LeadsWarehouse

DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeads = Connector(
    name="SmartLeads",
    description=DESCRIPTION,
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name="pull_leads",
            type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)


@pytest.fixture(autouse=True)
def reset_leads():
    LEADS_DB.clear()


@pytest.fixture
def manifest_directory():
    path = Path(__file__).parent
    yield path
    manifest = path / "manifest.json"
    manifest.unlink(missing_ok=True)


def test_connector_manifest():
    SmartLeads.manifest()


def test_hrflow_connectors_manifest(manifest_directory):
    manifest = Path(__file__).parent / "manifest.json"
    assert manifest.exists() is False

    connectors = [SmartLeads, SmartLeads]
    hrflow_connectors_manifest(connectors=connectors, directory_path=manifest_directory)

    assert manifest.exists() is True
    assert len(json.loads(manifest.read_text())["connectors"]) == len(connectors)


def test_connector_failures():
    campaign_id = "camp_xxx1"
    result = SmartLeads.pull_leads(
        action_parameters=dict(format=1),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.bad_action_parameters

    result = SmartLeads.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(gender="M"),
        target_parameters=dict(),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.bad_origin_parameters

    result = SmartLeads.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.bad_target_parameters

    result = SmartLeads.pull_leads(
        action_parameters=dict(format=lambda user: 10 / 0),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.format_failure

    result = SmartLeads.pull_leads(
        action_parameters=dict(logics=[lambda user: 10 / 0]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.logics_failure


def test_origin_warehouse_failure():
    connector = Connector(
        name="SmartLeads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=BadUsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    result = connector.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id="camp_xxx1"),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.read_failure
    assert result.run_stats[ActionRunEvents.read_success] == 0
    assert result.run_stats[ActionRunEvents.read_failure] == 1


def test_origin_not_readable_failure():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_leads",
                    type=WorkflowType.pull,
                    description="Send users as leads",
                    parameters=BaseActionParameters,
                    origin=LeadsWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("origin",)
    assert errors[0]["msg"] == "Origin warehouse is not readable"


def test_target_warehouse_failure():
    connector = Connector(
        name="SmartLeads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=BadLeadsWarehouse,
            ),
        ],
    )
    result = connector.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id="camp_xxx1"),
    )
    assert result.status == ActionStatus.fatal
    assert result.fatal_reason == FatalError.write_failure
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert (
        result.run_stats[ActionRunEvents.write_failure]
        == result.run_stats[ActionRunEvents.read_success]
    )


def test_target_not_writable_failure():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_leads",
                    type=WorkflowType.pull,
                    description="Send users as leads",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=UsersWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("target",)
    assert errors[0]["msg"] == "Target warehouse is not writable"


def test_connector_simple_success():
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == 0
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])


def test_connector_with_format():
    campaign_id = "camp_xxx1"

    def format(user):
        user["name"] = user["name"] + "_formatted"
        return user

    assert len(LEADS_DB[campaign_id]) == 0
    result = SmartLeads.pull_leads(
        action_parameters=dict(format=format),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == 0
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])

    assert [lead["name"].endswith("_formatted") for lead in LEADS_DB[campaign_id]]


def test_connector_with_logics():
    campaign_id = "camp_xxx1"
    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
        action_parameters=dict(logics=[lambda u: u, lambda u: None, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(LEADS_DB[campaign_id]) == 0

    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
        action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: None]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(LEADS_DB[campaign_id]) == 0

    # With pass-through logics
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
        action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == 0
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])

    # With only males logic
    campaign_id = "camp_xxx2"
    n_males = len([u for u in USERS_DB if u["gender"] == "male"])

    def logic(user):
        if user["gender"] == "male":
            return user

    assert len(LEADS_DB[campaign_id]) == 0
    result = SmartLeads.pull_leads(
        action_parameters=dict(logics=[lambda u: u, logic, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == len(USERS_DB) - n_males
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) != len(LEADS_DB[campaign_id])
    assert n_males == len(LEADS_DB[campaign_id])


def test_connector_default_format():
    def smarter_format(user):
        user["name"] = user["name"].upper()
        return user

    EvenSmarterLeads = Connector(
        name="SmartLeads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters.with_default_format(
                    "even_smarter_leads_connector", smarter_format
                ),
                origin=UsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    # Without default format
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0
    result = EvenSmarterLeads.pull_leads(
        action_parameters=dict(format=lambda user: user),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == 0
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert any(lead["name"].upper() != lead["name"] for lead in LEADS_DB[campaign_id])

    # With default format
    campaign_id = "camp_xxx2"
    assert len(LEADS_DB[campaign_id]) == 0
    result = EvenSmarterLeads.pull_leads(
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == ActionStatus.success
    assert result.run_stats[ActionRunEvents.read_success] == len(USERS_DB)
    assert result.run_stats[ActionRunEvents.format_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_failure] == 0
    assert result.run_stats[ActionRunEvents.logics_discard] == 0
    assert result.run_stats[ActionRunEvents.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert all(lead["name"].upper() == lead["name"] for lead in LEADS_DB[campaign_id])


def test_action_run_results_from_run_stats():
    result = ActionRunResult.from_run_stats(
        Counter({ActionRunEvents.read_success: 0, ActionRunEvents.read_failure: 0})
    )
    assert result.status is ActionStatus.success
    assert result.fatal_reason is FatalError.none

    result = ActionRunResult.from_run_stats(
        Counter({ActionRunEvents.read_success: 0, ActionRunEvents.read_failure: 1})
    )
    assert result.status is ActionStatus.fatal
    assert result.fatal_reason is FatalError.read_failure

    result = ActionRunResult.from_run_stats(
        Counter({ActionRunEvents.read_success: 5, ActionRunEvents.read_failure: 1})
    )
    assert result.status is ActionStatus.success_with_failures
    assert result.fatal_reason is FatalError.none

    result = ActionRunResult.from_run_stats(
        Counter(
            {
                ActionRunEvents.read_success: 5,
                ActionRunEvents.read_failure: 1,
                ActionRunEvents.format_failure: 5,
            }
        )
    )
    assert result.status is ActionStatus.fatal
    assert result.fatal_reason is FatalError.format_failure

    result = ActionRunResult.from_run_stats(
        Counter(
            {
                ActionRunEvents.read_success: 5,
                ActionRunEvents.read_failure: 1,
                ActionRunEvents.format_failure: 1,
                ActionRunEvents.logics_failure: 4,
            }
        )
    )
    assert result.status is ActionStatus.fatal
    assert result.fatal_reason is FatalError.logics_failure

    result = ActionRunResult.from_run_stats(
        Counter(
            {
                ActionRunEvents.read_success: 5,
                ActionRunEvents.read_failure: 1,
                ActionRunEvents.format_failure: 1,
                ActionRunEvents.logics_discard: 1,
                ActionRunEvents.logics_failure: 3,
            }
        )
    )
    assert result.status is ActionStatus.success_with_failures
    assert result.fatal_reason is FatalError.none

    result = ActionRunResult.from_run_stats(
        Counter(
            {
                ActionRunEvents.read_success: 5,
                ActionRunEvents.read_failure: 1,
                ActionRunEvents.format_failure: 1,
                ActionRunEvents.logics_discard: 1,
                ActionRunEvents.logics_failure: 1,
                ActionRunEvents.write_failure: 2,
            }
        )
    )
    assert result.status is ActionStatus.fatal
    assert result.fatal_reason is FatalError.write_failure

    result = ActionRunResult.from_run_stats(
        Counter(
            {
                ActionRunEvents.read_success: 5,
                ActionRunEvents.read_failure: 1,
                ActionRunEvents.format_failure: 1,
                ActionRunEvents.logics_discard: 1,
                ActionRunEvents.logics_failure: 1,
                ActionRunEvents.write_failure: 1,
            }
        )
    )
    assert result.status is ActionStatus.success_with_failures
    assert result.fatal_reason is FatalError.none
