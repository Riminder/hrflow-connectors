import json
from pathlib import Path

import pytest

from hrflow_connectors import hrflow_connectors_manifest
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.core.connector import ActionStatus
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
            source=UsersWarehouse,
            destination=LeadsWarehouse,
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
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(format=1),
            source_parameters=dict(),
            destination_parameters=dict(),
        )
        is ActionStatus.bad_action_parameters
    )
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(gender="M"),
            destination_parameters=dict(),
        )
        is ActionStatus.bad_source_parameters
    )
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(),
        )
        is ActionStatus.bad_destination_parameters
    )
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(format=lambda user: 10 / 0),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.format_failure
    )
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(logics=[lambda user: 10 / 0]),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.logics_failure
    )


def test_source_warehouse_failure():
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
                source=BadUsersWarehouse,
                destination=LeadsWarehouse,
            ),
        ],
    )
    assert (
        connector.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id="camp_xxx1"),
        )
        is ActionStatus.pulling_failure
    )


def test_source_not_pullable_failure():
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
                source=LeadsWarehouse,
                destination=LeadsWarehouse,
            ),
        ],
    )
    assert (
        connector.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id="camp_xxx1"),
        )
        is ActionStatus.source_not_pullable_failure
    )


def test_destination_warehouse_failure():
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
                source=UsersWarehouse,
                destination=BadLeadsWarehouse,
            ),
        ],
    )
    assert (
        connector.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id="camp_xxx1"),
        )
        is ActionStatus.pushing_failure
    )


def test_destination_not_pushable_failure():
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
                source=UsersWarehouse,
                destination=UsersWarehouse,
            ),
        ],
    )
    assert (
        connector.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(),
        )
        is ActionStatus.destination_not_pushable_failure
    )


def test_connector_simple_success():
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) == len(LEADS_DB[campaign_id])


def test_connector_with_format():
    campaign_id = "camp_xxx1"

    def format(user):
        user["name"] = user["name"] + "_formatted"
        return user

    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(format=format),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert [lead["name"].endswith("_formatted") for lead in LEADS_DB[campaign_id]]


def test_connector_with_logics():
    campaign_id = "camp_xxx1"
    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(logics=[lambda u: u, lambda u: None, lambda u: u]),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(LEADS_DB[campaign_id]) == 0

    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: None]),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(LEADS_DB[campaign_id]) == 0

    # With pass-through logics
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: u]),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) == len(LEADS_DB[campaign_id])

    # With only males logic
    campaign_id = "camp_xxx2"

    def logic(user):
        if user["gender"] == "male":
            return user

    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        SmartLeads.pull_leads(
            action_parameters=dict(logics=[lambda u: u, logic, lambda u: u]),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) != len(LEADS_DB[campaign_id])
    assert len([u for u in USERS_DB if u["gender"] == "male"]) == len(
        LEADS_DB[campaign_id]
    )


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
                source=UsersWarehouse,
                destination=LeadsWarehouse,
            ),
        ],
    )
    # Without default format
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        EvenSmarterLeads.pull_leads(
            action_parameters=dict(format=lambda user: user),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert any(lead["name"].upper() != lead["name"] for lead in LEADS_DB[campaign_id])

    # With default format
    campaign_id = "camp_xxx2"
    assert len(LEADS_DB[campaign_id]) == 0
    assert (
        EvenSmarterLeads.pull_leads(
            action_parameters=dict(),
            source_parameters=dict(),
            destination_parameters=dict(campaign_id=campaign_id),
        )
        is ActionStatus.success
    )
    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert all(lead["name"].upper() == lead["name"] for lead in LEADS_DB[campaign_id])
