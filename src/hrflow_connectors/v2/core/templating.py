from __future__ import annotations

import inspect
import typing as t
from dataclasses import dataclass

from hrflow_connectors.v2.core.common import Direction
from hrflow_connectors.v2.core.context import MAIN_IMPORT_NAME
from hrflow_connectors.v2.core.hrflow import HrFlowWarehouse
from hrflow_connectors.v2.core.msgspec_pydantic_compat import fields
from hrflow_connectors.v2.core.templates import Templates
from hrflow_connectors.v2.core.utils import get_import_name, reindent_function_source

if t.TYPE_CHECKING:
    from hrflow_connectors.v2.core.connector import Connector, Flow  # pragma: nocover


@dataclass(frozen=True)
class WORKFLOW:
    WORKFLOW_ID_SETTINGS_KEY = "__workflow_id"
    INCREMENTAL_SETTINGS_KEY = "__incremental"

    CONNECTOR_AUTH_SETTINGS_PREFIX = "connector_auth_"
    HRFLOW_AUTH_SETTINGS_PREFIX = "hrflow_auth_"
    PULL_PARAMETERS_SETTINGS_PREFIX = "pull_parameters_"
    PUSH_PARAMETERS_SETTINGS_PREFIX = "push_parameters_"

    LOGICS_PLACEHOLDER = "# << logics_placeholder >>"
    FORMAT_PLACEHOLDER = "# << format_placeholder >>"
    CALLBACK_PLACEHOLDER = "# << callback_placeholder >>"
    EVENT_PARSER_PLACEHOLDER = "# << event_parser_placeholder >>"

    ACTIVATE_INCREMENTAL = "enable"
    DEFAULT_EVENT_PARSER_FUNCTION_NAME = "default_event_parser"
    USER_EVENT_PARSER_FUNCTION_NAME = "event_parser"
    LOGICS_FUNCTIONS_NAME = "logics"
    FORMAT_FUNCTION_NAME = "format"
    CALLBACK_FUNCTION_NAME = "callback"


def workflow(
    connector: Connector,
    flow: Flow,
    integration: t.Literal["catch", "pull"],
) -> str:
    connector_aisle = connector.warehouse.get_aisle(flow.entity)
    hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)

    # This is only called with a properly validated
    # connector for which below must be true
    assert connector_aisle is not None
    assert hrflow_aisle is not None

    if flow.direction is Direction.inbound:
        origin_parameters = connector_aisle.parameters("read", flow.mode)
        target_parameters = hrflow_aisle.parameters("write", flow.mode)
    else:
        origin_parameters = hrflow_aisle.parameters("read", flow.mode)
        target_parameters = connector_aisle.parameters("write", flow.mode)

    # This is only called with a properly validated
    # connector for which below must be true
    assert origin_parameters is not None
    assert target_parameters is not None

    default_event_parser = ""
    if flow.event_parser is not None:
        default_event_parser = inspect.getsource(flow.event_parser).replace(
            flow.event_parser.__name__, WORKFLOW.DEFAULT_EVENT_PARSER_FUNCTION_NAME
        )
        default_event_parser = reindent_function_source(
            default_event_parser,
            function_name=WORKFLOW.DEFAULT_EVENT_PARSER_FUNCTION_NAME,
        )

    return Templates.get_template("workflow.py.j2").render(
        main_module=MAIN_IMPORT_NAME.get(),
        import_name=get_import_name(connector),
        workflow_id_settings_key=WORKFLOW.WORKFLOW_ID_SETTINGS_KEY,
        incremental_settings_key=WORKFLOW.INCREMENTAL_SETTINGS_KEY,
        activate_incremental_token=WORKFLOW.ACTIVATE_INCREMENTAL,
        connector_auth_settings_prefix=WORKFLOW.CONNECTOR_AUTH_SETTINGS_PREFIX,
        hrflow_auth_settings_prefix=WORKFLOW.HRFLOW_AUTH_SETTINGS_PREFIX,
        pull_parameters_settings_prefix=WORKFLOW.PULL_PARAMETERS_SETTINGS_PREFIX,
        push_parameters_settings_prefix=WORKFLOW.PUSH_PARAMETERS_SETTINGS_PREFIX,
        logics_placeholder=WORKFLOW.LOGICS_PLACEHOLDER,
        logics_functions_name=WORKFLOW.LOGICS_FUNCTIONS_NAME,
        format_placeholder=WORKFLOW.FORMAT_PLACEHOLDER,
        format_function_name=WORKFLOW.FORMAT_FUNCTION_NAME,
        callback_placeholder=WORKFLOW.CALLBACK_PLACEHOLDER,
        callback_function_name=WORKFLOW.CALLBACK_FUNCTION_NAME,
        event_parser_placeholder=WORKFLOW.EVENT_PARSER_PLACEHOLDER,
        default_event_parser=default_event_parser,
        user_event_parser_function_name=WORKFLOW.USER_EVENT_PARSER_FUNCTION_NAME,
        default_event_parser_function_name=WORKFLOW.DEFAULT_EVENT_PARSER_FUNCTION_NAME,
        action_name=flow.name(connector.subtype),
        type=integration,
        connector_auth=fields(connector.warehouse.auth),
        hrflow_auth=fields(HrFlowWarehouse.auth),
        pull_parameters=fields(origin_parameters),
        push_parameters=fields(target_parameters),
    )
