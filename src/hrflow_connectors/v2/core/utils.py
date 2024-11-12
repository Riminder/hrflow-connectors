from __future__ import annotations

import importlib
import inspect
import typing as t

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
