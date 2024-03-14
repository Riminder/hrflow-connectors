import logging
import random
import re
from contextlib import contextmanager
from datetime import date, datetime, time, timezone
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
| Name    | Type       | Available   | Release date  | Last update  | Pull profile list action | Pull job list action | Push profile action | Push job action | Catch profile action |
|----------------|--------------|----------|----------|----------|------------|--------|-----------|--------------|--------------|
| [**Smart Leads**](./src/hrflow_connectors/connectors/smartleads/README.md) | HCM | :white_check_mark: | *27/09/2021* | *04/09/2022* | :x: | :white_check_mark: | :white_check_mark: | :x: | :x: |
| [**No Connector Dir**](./src/hrflow_connectors/connectors/noconnectordir/README.md) | HCM | :white_check_mark: | *20/01/2019* | *14/03/2022* | :x: | :white_check_mark: | :white_check_mark: | :x: | :x: |


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
                        "2023-08-03T11:21:52+00:00 some/file.suff",
                        "2023-04-10T10:06:47+00:00 some/other_file.suff",
                        "2023-01-12T14:56:41+01:00 file.suff",
                    ]
                ),
                **kwargs,
            }
        ),
    ):
        yield


NOTEBOOKS_FILE = "anyfile.txt"


@pytest.fixture
def root_readme():
    return Path(__file__).parent / "README.md"


@pytest.fixture
def connectors_directory(root_readme: Path):
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


def test_documentation_adds_keep_empty_notebooks_file_if_folder_is_empty(
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


def test_documentation_does_not_add_keep_empty_notebooks_file_if_folder_has_other_files(
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


def test_documentation_removes_keep_empty_notebooks_file_if_folder_has_other_files(
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
        with patched_subprocess():
            generate_docs(
                connectors=[SmartLeads], connectors_directory=connectors_directory
            )


def test_main_readme_update_at_expected_value(root_readme, connectors_directory):
    connectors = [SmartLeads]

    dates = [
        date(year=2023, month=random.randint(1, 12), day=random.randint(1, 28))
        for _ in range(5)
    ]

    expected = max(dates)
    assert expected.strftime("%d/%m/%Y") not in root_readme.read_text()

    stdout = "\n".join(
        [
            datetime.combine(
                date,
                time(
                    random.randint(0, 23), random.randint(0, 59), random.randint(0, 59)
                ),
                tzinfo=timezone.utc,
            ).isoformat()
            + " some/file."
            + "".join(random.choices("abcdefghk", k=3))
            for date in dates
        ]
    )
    with patched_subprocess(stdout=stdout):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert expected.strftime("%d/%m/%Y") in root_readme.read_text()


IGNORED_PATHS = [
    "notebooks/{}".format(KEEP_EMPTY_NOTEBOOKS),
    "README.md",
    "test-config.yaml",
    "logo.png",
    "docs/pull_action.md",
]


def test_ignored_path_are_not_taken_into_account_for_main_readme_updated_at(
    root_readme, connectors_directory
):
    connectors = [SmartLeads]

    dates = [
        date(year=2023, month=random.randint(1, 12), day=random.randint(1, 28))
        for _ in range(5)
    ]

    max_of_dates = max(dates)
    greater_than_max_of_dates = date(
        year=2024, month=random.randint(1, 12), day=random.randint(1, 28)
    )

    assert greater_than_max_of_dates > max_of_dates

    assert greater_than_max_of_dates.strftime("%d/%m/%Y") not in root_readme.read_text()
    assert max_of_dates.strftime("%d/%m/%Y") not in root_readme.read_text()

    base_stdout = "\n".join(
        [
            datetime.combine(
                date,
                time(
                    random.randint(0, 23), random.randint(0, 59), random.randint(0, 59)
                ),
                tzinfo=timezone.utc,
            ).isoformat()
            + " some/file."
            + "".join(random.choices("abcdefghk", k=3))
            for date in dates
        ]
    )
    should_be_ignored = "\n".join(
        [
            datetime.combine(
                greater_than_max_of_dates, time.min, tzinfo=timezone.utc
            ).isoformat()
            + " "
            + ignored
            for ignored in IGNORED_PATHS
        ]
    )
    with patched_subprocess(stdout=base_stdout + "\n" + should_be_ignored):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert greater_than_max_of_dates.strftime("%d/%m/%Y") not in root_readme.read_text()
    assert max_of_dates.strftime("%d/%m/%Y") in root_readme.read_text()

    greater_than_max_of_dates_with_regular_file = "\n{} {}".format(
        datetime.combine(
            greater_than_max_of_dates, time.min, tzinfo=timezone.utc
        ).isoformat(),
        "regular/file.txt",
    )
    with patched_subprocess(
        stdout=base_stdout + greater_than_max_of_dates_with_regular_file
    ):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert greater_than_max_of_dates.strftime("%d/%m/%Y") in root_readme.read_text()
    assert max_of_dates.strftime("%d/%m/%Y") not in root_readme.read_text()


def test_main_readme_update_at_helper_doesnt_override_handwritten_updated_at(
    root_readme, connectors_directory
):
    connectors = [SmartLeads]

    set_at = date(year=2026, month=1, day=1)
    assert set_at.strftime("%d/%m/%Y") not in root_readme.read_text()

    stdout = "\n {} {}".format(
        datetime.combine(
            set_at,
            time.min,
            tzinfo=timezone.utc,
        ).isoformat(),
        "regular_file.txt",
    )
    with patched_subprocess(stdout=stdout):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert set_at.strftime("%d/%m/%Y") in root_readme.read_text()

    dates_before_set_at = [
        date(year=2023, month=random.randint(1, 12), day=random.randint(1, 28))
        for _ in range(5)
    ]
    for date_before in dates_before_set_at:
        assert date_before < set_at

    expected = max(dates_before_set_at)
    assert expected.strftime("%d/%m/%Y") not in root_readme.read_text()

    stdout = "\n".join(
        [
            datetime.combine(
                date,
                time(
                    random.randint(0, 23), random.randint(0, 59), random.randint(0, 59)
                ),
                tzinfo=timezone.utc,
            ).isoformat()
            + " some/file."
            + "".join(random.choices("abcdefghk", k=3))
            for date in dates_before_set_at
        ]
    )
    with patched_subprocess(stdout=stdout):
        generate_docs(connectors=connectors, connectors_directory=connectors_directory)

    assert expected.strftime("%d/%m/%Y") not in root_readme.read_text()
    assert set_at.strftime("%d/%m/%Y") in root_readme.read_text()


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
        with patched_subprocess():
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
        with patched_subprocess():
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
