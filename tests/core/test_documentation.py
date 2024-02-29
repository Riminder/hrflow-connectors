import logging
import re
from contextlib import contextmanager
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
    ConnectorType,
    WorkflowType,
)
from hrflow_connectors.core.documentation import (
    KEEP_EMPTY_NOTEBOOKS,
    USE_REMOTE_REV,
    InvalidConnectorReadmeFormat,
)
from tests.core.src.hrflow_connectors.connectors.localusers.warehouse import (
    UsersWarehouse,
)
from tests.core.src.hrflow_connectors.connectors.smartleads.warehouse import (
    LeadsWarehouse,
)

DUMMY_ROOT_README = """
# Test README used for documentation tests

# 🤝 List of Connectors (ATS/CRM/HCM)
| Name    | Type       | Available   | Release date  | Last update  | Pull profile list action | Pull job list action | Push profile action | Push job action |
|----------------|--------------|----------|----------|----------|------------|--------|-----------|--------------|
| [**Smart Leads**](./src/hrflow_connectors/connectors/smartleads/README.md) | HCM | :white_check_mark: | *27/09/2022* | *04/09/2023* | :x: | :white_check_mark: | :white_check_mark: | :x: |
| [**No Connector Dir**](./src/hrflow_connectors/connectors/noconnectordir/README.md) | HCM | :white_check_mark: | *20/01/2019* | *14/03/2022* | :x: | :white_check_mark: | :white_check_mark: | :x: |


- :white_check_mark: : Done
- :hourglass: : Work in progress
- 🎯 : Backlog

"""
DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeads = Connector(
    name="SmartLeads",
    type=ConnectorType.Other,
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


@contextmanager
def patched_subprocess(**kwargs):
    with mock.patch(
        "hrflow_connectors.core.documentation.subprocess.run",
        return_value=mock.MagicMock(
            **{
                "stderr": None,
                "stdout": "\n".join(
                    [
                        "2023-08-03T11:21:52+00:00",
                        "2023-04-10T10:06:47+00:00",
                        "2023-01-12T14:56:41+01:00",
                    ]
                ),
                **kwargs,
            }
        ),
    ):
        yield


NOTEBOOKS_FILE = "anyfile.txt"


@pytest.fixture
def connectors_directory():
    root_readme = Path(__file__).parent / "README.md"
    root_readme.write_bytes(DUMMY_ROOT_README.encode())

    path = Path(__file__).parent / "src" / "hrflow_connectors" / "connectors"

    yield path

    try:
        root_readme.unlink()
    except FileNotFoundError:
        pass

    readme = path / SmartLeads.model.name.lower() / "README.md"
    notebooks_directory = path / SmartLeads.model.name.lower() / "notebooks"
    keep_empty_notebooks_file = (
        path / SmartLeads.model.name.lower() / "notebooks" / KEEP_EMPTY_NOTEBOOKS
    )
    notebook = notebooks_directory / NOTEBOOKS_FILE
    actions_documentation_directory = path / SmartLeads.model.name.lower() / "docs"
    action_documentation = actions_documentation_directory / "{}.md".format(
        SmartLeads.model.actions[0].name.value
    )

    for file in [readme, action_documentation, keep_empty_notebooks_file, notebook]:
        try:
            file.unlink()
        except FileNotFoundError:
            pass

    for directory in [actions_documentation_directory, notebooks_directory]:
        if directory.is_dir():
            directory.rmdir()


def test_documentation(connectors_directory):
    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    notebooks_directory = (
        connectors_directory / SmartLeads.model.name.lower() / "notebooks"
    )
    keep_empty_notebooks_file = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "notebooks"
        / KEEP_EMPTY_NOTEBOOKS
    )
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert action_documentation.exists() is False

    connectors = [SmartLeads]
    with patched_subprocess():
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert readme.exists() is True
    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is True
    assert action_documentation.exists() is True


def test_documentation_adds_keep_empty_notebooks_fils_if_folder_is_empty(
    connectors_directory,
):
    notebooks_directory = (
        connectors_directory / SmartLeads.model.name.lower() / "notebooks"
    )
    keep_empty_notebooks_file = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "notebooks"
        / KEEP_EMPTY_NOTEBOOKS
    )

    notebooks_directory.mkdir()

    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    connectors = [SmartLeads]
    with patched_subprocess():
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is True

    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )
    assert readme.exists() is True
    assert action_documentation.exists() is True


def test_documentation_does_not_add_keep_empty_notebooks_fils_if_folder_has_other_files(
    connectors_directory,
):
    notebooks_directory = (
        connectors_directory / SmartLeads.model.name.lower() / "notebooks"
    )
    keep_empty_notebooks_file = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "notebooks"
        / KEEP_EMPTY_NOTEBOOKS
    )

    notebooks_directory.mkdir()
    other = notebooks_directory / NOTEBOOKS_FILE
    other.touch()

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    connectors = [SmartLeads]
    with patched_subprocess():
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )
    assert readme.exists() is True
    assert action_documentation.exists() is True


def test_documentation_removes_keep_empty_notebooks_fils_if_folder_has_other_files(
    connectors_directory,
):
    notebooks_directory = (
        connectors_directory / SmartLeads.model.name.lower() / "notebooks"
    )
    keep_empty_notebooks_file = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "notebooks"
        / KEEP_EMPTY_NOTEBOOKS
    )

    notebooks_directory.mkdir()
    keep_empty_notebooks_file.touch()
    other = notebooks_directory / NOTEBOOKS_FILE
    other.touch()

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is True

    connectors = [SmartLeads]
    with patched_subprocess():
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    action_documentation = (
        connectors_directory
        / SmartLeads.model.name.lower()
        / "docs"
        / "{}.md".format(SmartLeads.model.actions[0].name.value)
    )
    assert readme.exists() is True
    assert action_documentation.exists() is True


def test_documentation_fails_if_actions_section_not_found(connectors_directory):
    readme = connectors_directory / SmartLeads.model.name.lower() / "README.md"
    with patched_subprocess():
        generate_docs(
            connectors=[SmartLeads], connectors_directory=connectors_directory
        )

    content = readme.read_text()
    content = content.replace(
        "# 🔌 Connector Actions", "This breaks the expect section start"
    )
    readme.write_bytes(content.encode())

    with pytest.raises(InvalidConnectorReadmeFormat):
        with mock.patch(
            "hrflow_connectors.core.documentation.subprocess.run",
            return_value=mock.MagicMock(
                stderr=None,
                stdout="\n".join(
                    [
                        "2023-08-03T11:21:52+00:00",
                        "2023-04-10T10:06:47+00:00",
                        "2023-01-12T14:56:41+01:00",
                    ]
                ),
            ),
        ):
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
        with mock.patch(
            "hrflow_connectors.core.documentation.subprocess.run",
            return_value=mock.MagicMock(
                stderr=None,
                stdout="\n".join(
                    [
                        "2023-08-03T11:21:52+00:00",
                        "2023-04-10T10:06:47+00:00",
                        "2023-01-12T14:56:41+01:00",
                    ]
                ),
            ),
        ):
            generate_docs(
                connectors=connectors, connectors_directory=connectors_directory
            )

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
        with mock.patch(
            "hrflow_connectors.core.documentation.subprocess.run",
            return_value=mock.MagicMock(
                stderr=None,
                stdout="\n".join(
                    [
                        "2023-08-03T11:21:52+00:00",
                        "2023-04-10T10:06:47+00:00",
                        "2023-01-12T14:56:41+01:00",
                    ]
                ),
            ),
        ):
            generate_docs(
                connectors=connectors, connectors_directory=connectors_directory
            )

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
        type=ConnectorType.Other,
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
    with patched_subprocess():
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert readme.exists() is False
    assert action_documentation.exists() is False
    assert len(caplog.records) == 1
    assert caplog.record_tuples[0][1] == logging.ERROR
    assert caplog.record_tuples[0][2].startswith(
        "Skipping documentation for {}: no directory found at".format(mismatch_name)
    )


def test_documentation_fails_if_root_readme_not_found(connectors_directory):
    (connectors_directory / ".." / ".." / ".." / "README.md").unlink()
    with patched_subprocess():
        with pytest.raises(Exception) as excinfo:
            generate_docs(
                connectors=[SmartLeads], connectors_directory=connectors_directory
            )

    assert excinfo.value.args[0].startswith("Failed to find root README")


def test_documentation_fails_if_subprocess_has_stderr(connectors_directory):
    stderr = "FATAL ERROR"
    with patched_subprocess(stderr=stderr):
        with pytest.raises(Exception) as excinfo:
            generate_docs(
                connectors=[SmartLeads], connectors_directory=connectors_directory
            )

    assert (
        excinfo.value.args[0].startswith("Subprocess run for Git update dates failed")
        and stderr in excinfo.value.args[0]
    )


def test_documentation_fails_if_connector_not_already_listed_in_root_readme(
    connectors_directory,
):
    name = "Not Listed In Root README"
    NotListed = Connector(
        name=name,
        type=ConnectorType.Other,
        description=DESCRIPTION,
        url="https://not.listed.in.root.test/",
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
    with patched_subprocess():
        with pytest.raises(Exception) as excinfo:
            generate_docs(
                connectors=[NotListed], connectors_directory=connectors_directory
            )

    assert (
        excinfo.value.args[0].startswith("Could not find listing for")
        and name in excinfo.value.args[0]
    )
