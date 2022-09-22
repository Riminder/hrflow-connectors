import logging
from pathlib import Path

import pytest

from hrflow_connectors import generate_docs
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from tests.core.localusers.warehouse import UsersWarehouse
from tests.core.smartleads.warehouse import LeadsWarehouse

DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeads = Connector(
    name="SmartLeads",
    description=DESCRIPTION,
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name="pull_leads",
            trigger_type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)


@pytest.fixture
def connectors_directory():
    path = Path(__file__).parent
    yield path
    readme = path / SmartLeads.model.name.lower() / "README.md"
    actions_documentation_directory = path / SmartLeads.model.name.lower() / "docs"
    action_documentation = actions_documentation_directory / "{}.md".format(
        SmartLeads.model.actions[0].name
    )

    readme.unlink(missing_ok=True)
    action_documentation.unlink(missing_ok=True)
    if actions_documentation_directory.is_dir():
        actions_documentation_directory.rmdir()


def test_documentation(connectors_directory):

    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name)
    )

    assert readme.exists() is False
    assert action_documentation.exists() is False

    connectors = [SmartLeads]
    generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert readme.exists() is True
    assert action_documentation.exists() is True


def test_documentation_connector_directory_not_found(caplog, connectors_directory):
    mismatch_name = "NoConnectorDir"
    NameMismatchSmartLeads = Connector(
        name=mismatch_name,
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )

    readme = (
        connectors_directory / NameMismatchSmartLeads.model.name.lower() / "README.md"
    )
    action_documentation = (
        connectors_directory
        / NameMismatchSmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(NameMismatchSmartLeads.model.actions[0].name)
    )

    assert readme.exists() is False
    assert action_documentation.exists() is False

    connectors = [NameMismatchSmartLeads]
    generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert readme.exists() is False
    assert action_documentation.exists() is False
    assert len(caplog.records) == 1
    assert caplog.record_tuples[0][1] == logging.ERROR
    assert caplog.record_tuples[0][2].startswith(
        "Skipping documentation for {}: no directory found at".format(mismatch_name)
    )
