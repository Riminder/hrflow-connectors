from __future__ import annotations

import logging
import typing as t
from pathlib import Path

from hrflow_connectors.v2.core import templating
from hrflow_connectors.v2.core.utils import CONNECTORS_DIRECTORY, get_import_name

if t.TYPE_CHECKING:
    from hrflow_connectors.v2.core.connector import Connector  # pragma: nocover

logger = logging.getLogger()

KEEP_EMPTY_FOLDER = ".gitkeep"


def ensure_gitkeep(directory: Path, gitkeep_filename: str = ".gitkeep") -> None:
    gitkeep_file = directory / gitkeep_filename
    create_empty_file = True

    if directory.is_dir():
        for child in directory.iterdir():
            if not child.name == gitkeep_file.name:
                create_empty_file = False
                try:
                    gitkeep_file.unlink()
                except FileNotFoundError:
                    pass
                break
    else:
        directory.mkdir(parents=True)

    if create_empty_file:
        gitkeep_file.touch()


def hrflow_connectors_docs(
    connectors: t.Iterable[Connector],
    connectors_directory: Path = CONNECTORS_DIRECTORY,
) -> None:
    for connector in connectors:
        # Done early to avoid writing file to disk if connector is
        # misconfigured
        get_import_name(connector)

        connector_directory = connectors_directory / connector.subtype
        if not connector_directory.is_dir():
            logger.error(
                "Skipping documentation for {}: no directory found at {}".format(
                    connector.name, connector_directory
                )
            )
            continue

        readme = connector_directory / "README.md"
        readme_content = None
        if readme.exists():
            readme_content = readme.read_text()

        readme.write_bytes(
            templating.connector_readme(
                connector, current_content=readme_content
            ).encode()
        )

        stubs = connector_directory / "connector.pyi"
        stubs.write_bytes(templating.connector_stub(connector).encode())

        notebooks_directory = connector_directory / "notebooks"
        ensure_gitkeep(notebooks_directory, KEEP_EMPTY_FOLDER)

        format_mappings_directory = connector_directory / "mappings" / "format"
        ensure_gitkeep(format_mappings_directory, KEEP_EMPTY_FOLDER)

        action_docs_directory = connector_directory / "docs"
        if not action_docs_directory.is_dir():
            action_docs_directory.mkdir()

        for flow in connector.flows:
            action_documentation = action_docs_directory / "{}.md".format(
                flow.name(connector.subtype)
            )
            action_documentation.write_bytes(
                templating.connector_action(connector, flow).encode()
            )
