import logging
import re
from os.path import relpath
from pathlib import Path
from unittest import mock

import pytest

from hrflow_connectors import generate_docs
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.core.documentation import (
    USE_REMOTE_REV,
    InvalidConnectorReadmeFormat,
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
            name=ActionName.pull_profile_list,
            action_type=ActionType.inbound,
            trigger_type=WorkflowType.pull,
            description="Test action",
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
        SmartLeads.model.actions[0].name.value
    )
    try:
        readme.unlink()
    except FileNotFoundError:
        pass
    try:
        action_documentation.unlink()
    except FileNotFoundError:
        pass
    if actions_documentation_directory.is_dir():
        actions_documentation_directory.rmdir()


def test_documentation(connectors_directory):
    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )

    assert readme.exists() is False
    assert action_documentation.exists() is False

    connectors = [SmartLeads]
    generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert readme.exists() is True
    assert action_documentation.exists() is True


def test_documentation_fails_if_actions_section_not_found(connectors_directory):
    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"

    generate_docs(connectors=[SmartLeads], connectors_directory=connectors_directory)

    content = readme.read_text()
    content = content.replace(
        "# 🔌 Connector Actions", "This breaks the expect section start"
    )
    readme.write_bytes(content.encode())

    with pytest.raises(InvalidConnectorReadmeFormat):
        generate_docs(
            connectors=[SmartLeads], connectors_directory=connectors_directory
        )


def test_documentation_with_remote_code_links(connectors_directory):
    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )

    assert readme.exists() is False
    assert action_documentation.exists() is False

    reset_token = USE_REMOTE_REV.set("dummy_git_rev")

    connectors = [SmartLeads]
    with mock.patch(
        "hrflow_connectors.core.documentation.os.path.relpath",
        lambda a, b: relpath(a, b).replace(
            "hrflow_connectors/", "site-packages/hrflow_connectors/"
        ),
    ):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    links = re.findall(r"\[`\S+`\]\(\S+\)", action_documentation.read_text())
    assert len(links) > 0
    for link in links:
        assert link.split("](")[1].startswith(
            "https://github.com/Riminder/hrflow-connectors/tree/dummy_git_rev"
        )
        assert "site-packages/hrflow_connectors" not in link

    USE_REMOTE_REV.reset(reset_token)
    with mock.patch(
        "hrflow_connectors.core.documentation.os.path.relpath",
        lambda a, b: relpath(a, b).replace(
            "hrflow_connectors/", "site-packages/hrflow_connectors/"
        ),
    ):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    links = re.findall(r"\[`\S+`\]\(\S+\)", action_documentation.read_text())
    assert len(links) > 0
    for link in links:
        assert (
            "https://github.com/Riminder/hrflow-connectors/tree/dummy_git_rev"
            not in link
        )
        assert "site-packages/hrflow_connectors" in link


def test_documentation_connector_directory_not_found(caplog, connectors_directory):
    mismatch_name = "NoConnectorDir"
    NameMismatchSmartLeads = Connector(
        name=mismatch_name,
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.pull_profile_list,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
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
        / "{}.md".format(NameMismatchSmartLeads.model.actions[0].name.value)
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
