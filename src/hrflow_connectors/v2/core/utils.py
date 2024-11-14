from __future__ import annotations

import importlib
import inspect
import typing as t
from pathlib import Path

from hrflow_connectors.v2.core.context import MAIN_IMPORT_NAME

if t.TYPE_CHECKING:
    from hrflow_connectors.v2.core.connector import Connector  # pragma: nocover


class ConnectorImportNameNotFound(Exception):
    pass


class AmbiguousConnectorImportName(Exception):
    pass


def get_import_name(connector: Connector) -> str:
    main_module = importlib.import_module(MAIN_IMPORT_NAME.get())

    members = inspect.getmembers(main_module, lambda s: s is connector)
    if len(members) == 0:
        raise ConnectorImportNameNotFound(
            "Failed to find import name for"
            f" connector {connector.name}\nNo match found for"
            " below members"
            f" {[symbol for symbol, _ in inspect.getmembers(main_module)]}"
        )
    if len(members) > 1:
        raise AmbiguousConnectorImportName(
            "Found multiple import names for"
            f" connector {connector.name}\n"
            f" {[symbol for symbol, _ in members]}"
        )
    return members[0][0]


class NoDefFunctionNameFound(Exception):
    pass


def reindent_function_source(source: str, function_name: str):
    search_for = f"def {function_name}"
    def_line = next(
        (line for line in source.splitlines() if search_for in line),
        None,
    )
    if def_line is None:
        raise NoDefFunctionNameFound()

    start_lines_from = def_line.find(search_for)
    return "\n".join([line[start_lines_from:] for line in source.splitlines()])


KB = 1024
MAX_LOGO_SIZE_BYTES = 100 * KB
MAX_LOGO_PIXEL = 150
MIN_LOGO_PIXEL = 34
HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE = (
    "https://raw.githubusercontent.com/Riminder/hrflow-connectors"
)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"
ROOT_DIRECTORY = Path(__file__).parent.parent.parent.parent.parent


def compute_logo_path(
    name: str, subtype: str, connectors_directory: Path = CONNECTORS_DIRECTORY
) -> str:
    try:
        from PIL import Image, UnidentifiedImageError
    except ModuleNotFoundError:  # pragma: no cover
        raise Exception(
            "PIL is not found in current environment. Mind that you need to install"
            " the package with dev dependencies to use manifest utility"
        )

    connector_directory = connectors_directory / subtype
    if not connector_directory.is_dir():
        raise ValueError(
            "No directory found for connector {} in {}".format(
                name, connector_directory
            )
        )

    logo_paths = list(connector_directory.glob("logo.*"))
    if len(logo_paths) == 0:
        raise ValueError(
            "Missing logo for connector {}. Add a logo file at {} named"
            " 'logo.(png|jpeg|...)'".format(name, connector_directory)
        )
    elif len(logo_paths) > 1:
        raise ValueError(
            "Found multiple logos for connector {} => {}. Only a single one should"
            " be present".format(name, logo_paths)
        )
    logo = logo_paths[0]
    size = logo.lstat().st_size
    if size > MAX_LOGO_SIZE_BYTES:
        raise ValueError(
            "Logo size {} KB for connector {} is above maximum limit of {} KB".format(
                size // KB, name, MAX_LOGO_SIZE_BYTES // KB
            )
        )
    try:
        width, height = Image.open(logo).size
    except UnidentifiedImageError:
        raise ValueError(
            "Logo file for connector {} at {} doesn't seem to be a valid image".format(
                name, logo
            )
        )

    if width != height or width > MAX_LOGO_PIXEL or width < MIN_LOGO_PIXEL:
        raise ValueError(
            "Bad logo dimensions of ({}, {}) for connector {}. Logo should have"
            " square dimensions within range {min}x{min} {max}x{max}".format(
                width,
                height,
                name,
                min=MIN_LOGO_PIXEL,
                max=MAX_LOGO_PIXEL,
            )
        )
    return f"{HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE}/master/{logo.relative_to(ROOT_DIRECTORY)}"  # noqa E501
