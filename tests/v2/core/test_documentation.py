import logging
from pathlib import Path

import pytest

from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import (
    Flow,
)
from hrflow_connectors.v2.core.documentation import (
    KEEP_EMPTY_FOLDER,
    hrflow_connectors_docs,
)
from hrflow_connectors.v2.core.templating import InvalidConnectorReadmeContent
from hrflow_connectors.v2.core.utils import (
    AmbiguousConnectorImportName,
    ConnectorImportNameNotFound,
)
from tests.v2.core.conftest import SmartLeadsProto, TypedSmartLeads
from tests.v2.core.utils import added_connectors, main_import_name_as

NOTEBOOKS_FILE = "anyfile.txt"
FORMAT_FILE = "pull_profile_list.json"


@pytest.fixture(scope="function", autouse=True)
def clean(connectors_directory: Path, SmartLeads: TypedSmartLeads):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )
    notebook = notebooks_directory / NOTEBOOKS_FILE
    mappings_directory = connectors_directory / SmartLeads.subtype / "mappings"
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = format_mappings_directory / KEEP_EMPTY_FOLDER
    format_file = format_mappings_directory / FORMAT_FILE

    actions_documentation_directory = connectors_directory / SmartLeads.subtype / "docs"
    action_documentations = [
        actions_documentation_directory / f"{flow.name(SmartLeads.subtype)}.md"
        for flow in SmartLeads.flows
    ]

    for file in [
        readme,
        *action_documentations,
        keep_empty_notebooks_file,
        notebook,
        format_file,
        keep_empty_format_file,
    ]:
        try:
            file.unlink()
        except FileNotFoundError:
            pass

    for directory in [
        actions_documentation_directory,
        notebooks_directory,
        format_mappings_directory,
        mappings_directory,
    ]:
        if directory.is_dir():
            directory.rmdir()


def test_documentation(connectors_directory: Path, SmartLeads: TypedSmartLeads):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )
    action_documentations = [
        connectors_directory
        / SmartLeads.subtype
        / "docs"
        / f"{flow.name(SmartLeads.subtype)}.md"
        for flow in SmartLeads.flows
    ]

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False
    for action_documentation in action_documentations:
        assert action_documentation.exists() is False

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert readme.exists() is True
    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is True
    assert format_mappings_directory.exists() is True
    assert keep_empty_format_file.exists() is True
    for action_documentation in action_documentations:
        assert action_documentation.exists() is True
        assert (
            "from hrflow_connectors.v2 import SmartLeads"
            in action_documentation.read_text()
        )


def test_documentation_works_with_parameterized_main_module_name(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )
    action_documentations = [
        connectors_directory
        / SmartLeads.subtype
        / "docs"
        / f"{flow.name(SmartLeads.subtype)}.md"
        for flow in SmartLeads.flows
    ]

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False
    for action_documentation in action_documentations:
        assert action_documentation.exists() is False

    parameterized_name = "third_party"
    with main_import_name_as(parameterized_name):
        # Should fail because by default add_connectors adds names to
        # hrflow_connectors default import name
        with pytest.raises(ModuleNotFoundError):
            with added_connectors([("SmartLeads", SmartLeads)]):
                hrflow_connectors_docs(
                    connectors=[SmartLeads],
                    connectors_directory=connectors_directory,
                )

        with added_connectors(
            [("SmartLeads", SmartLeads)], parameterized_name, create_module=True
        ):
            hrflow_connectors_docs(
                connectors=[SmartLeads],
                connectors_directory=connectors_directory,
            )

    assert readme.exists() is True
    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is True
    assert format_mappings_directory.exists() is True
    assert keep_empty_format_file.exists() is True
    assert action_documentation.exists() is True

    for action_documentation in action_documentations:
        assert action_documentation.exists() is True
        assert (
            f"from {parameterized_name} import SmartLeads"
            in action_documentation.read_text()
        )


def test_documentation_adds_keep_empty_notebooks_file_if_folder_is_empty(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )

    notebooks_directory.mkdir()

    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert notebooks_directory.exists() is True
    assert keep_empty_notebooks_file.exists() is True

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_does_not_add_keep_empty_notebooks_file_if_folder_has_other_files(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )

    notebooks_directory.mkdir()
    other = notebooks_directory / NOTEBOOKS_FILE
    other.touch()

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_removes_keep_empty_notebooks_file_if_folder_has_other_files(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )

    notebooks_directory.mkdir()
    keep_empty_notebooks_file.touch()
    other = notebooks_directory / NOTEBOOKS_FILE
    other.touch()

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is True

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert notebooks_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_notebooks_file.exists() is False

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_adds_keep_empty_format_file_if_folder_is_empty(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    assert format_mappings_directory.exists() is True
    assert keep_empty_format_file.exists() is False

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert format_mappings_directory.exists() is True
    assert keep_empty_format_file.exists() is True

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_does_not_add_keep_empty_format_file_if_folder_has_other_files(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )
    format_mappings_directory.mkdir(parents=True, exist_ok=True)
    other = format_mappings_directory / FORMAT_FILE
    other.touch()

    assert format_mappings_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_format_file.exists() is False

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert format_mappings_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_format_file.exists() is False

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_removes_keep_empty_format_file_if_folder_has_other_files(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )

    format_mappings_directory.mkdir(parents=True, exist_ok=True)
    keep_empty_format_file.touch()
    other = format_mappings_directory / FORMAT_FILE
    other.touch()

    assert format_mappings_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_format_file.exists() is True

    with added_connectors([("SmartLeads", SmartLeads)]):
        hrflow_connectors_docs(
            connectors=[SmartLeads],
            connectors_directory=connectors_directory,
        )

    assert format_mappings_directory.exists() is True
    assert other.exists() is True
    assert keep_empty_format_file.exists() is False

    readme = connectors_directory / SmartLeads.subtype / "README.md"
    assert readme.exists() is True

    for flow in SmartLeads.flows:
        assert (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        ).exists() is True


def test_documentation_fails_if_cannot_find_import_name(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )
    action_documentations = [
        (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        )
        for flow in SmartLeads.flows
    ]
    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False
    for action_documentation in action_documentations:
        assert action_documentation.exists() is False

    with pytest.raises(ConnectorImportNameNotFound):
        hrflow_connectors_docs(
            connectors=[SmartLeads], connectors_directory=connectors_directory
        )

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False
    for action_documentation in action_documentations:
        assert action_documentation.exists() is False


def test_documentation_fails_if_connector_misconfigured(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    notebooks_directory = connectors_directory / SmartLeads.subtype / "notebooks"
    keep_empty_notebooks_file = (
        connectors_directory / SmartLeads.subtype / "notebooks" / KEEP_EMPTY_FOLDER
    )
    format_mappings_directory = (
        connectors_directory / SmartLeads.subtype / "mappings" / "format"
    )
    keep_empty_format_file = (
        connectors_directory
        / SmartLeads.subtype
        / "mappings"
        / "format"
        / KEEP_EMPTY_FOLDER
    )
    action_documentations = [
        (
            connectors_directory
            / SmartLeads.subtype
            / "docs"
            / f"{flow.name(SmartLeads.subtype)}.md"
        )
        for flow in SmartLeads.flows
    ]

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False
    for action_documentation in action_documentations:
        assert action_documentation.exists() is False

    with pytest.raises(AmbiguousConnectorImportName):
        with added_connectors([("SmartLeads", SmartLeads), ("Duplicated", SmartLeads)]):
            hrflow_connectors_docs(
                connectors=[SmartLeads], connectors_directory=connectors_directory
            )

    assert readme.exists() is False
    assert notebooks_directory.exists() is False
    assert keep_empty_notebooks_file.exists() is False
    assert format_mappings_directory.exists() is False
    assert keep_empty_format_file.exists() is False

    for action_documentation in action_documentations:
        assert action_documentation.exists() is False


def test_documentation_fails_if_actions_section_not_found(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    readme = connectors_directory / SmartLeads.subtype / "README.md"
    with added_connectors([("SmartLeads", SmartLeads)]):
        with added_connectors([("SmartLeads", SmartLeads)]):
            hrflow_connectors_docs(
                connectors=[SmartLeads],
                connectors_directory=connectors_directory,
            )

        content = readme.read_text()
        content = content.replace(
            "# 🔌 Connector Actions", "This breaks the expect section start"
        )
        readme.write_bytes(content.encode())

        with pytest.raises(InvalidConnectorReadmeContent):
            with added_connectors([("SmartLeads", SmartLeads)]):
                hrflow_connectors_docs(
                    connectors=[SmartLeads],
                    connectors_directory=connectors_directory,
                )


def test_documentation_connector_directory_not_found(
    caplog, connectors_directory: Path, SmartLeadsF: SmartLeadsProto
):
    mismatch_name = "NoConnectorDir"
    subtype = mismatch_name.lower().replace(" ", "")
    flow = Flow(Mode.create, Entity.job, Direction.inbound)
    NameMismatchSmartLeads = SmartLeadsF(
        name=mismatch_name, subtype=subtype, flows=(flow,)
    )

    readme = connectors_directory / NameMismatchSmartLeads.subtype / "README.md"
    assert readme.exists() is False

    action_documentation = (
        connectors_directory
        / NameMismatchSmartLeads.subtype
        / "docs"
        / f"{flow.name(subtype)}.md"
    )
    assert action_documentation.exists() is False

    connectors = [NameMismatchSmartLeads]
    with added_connectors([("NameMismatchSmartLeads", NameMismatchSmartLeads)]):
        hrflow_connectors_docs(
            connectors=connectors,
            connectors_directory=connectors_directory,
        )

    assert readme.exists() is False
    assert action_documentation.exists() is False

    assert len(caplog.records) == 1
    assert caplog.record_tuples[0][1] == logging.ERROR
    assert caplog.record_tuples[0][2].startswith(
        "Skipping documentation for {}: no directory found at".format(mismatch_name)
    )
