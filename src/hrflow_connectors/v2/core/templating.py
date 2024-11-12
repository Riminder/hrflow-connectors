from __future__ import annotations

import inspect
import typing as t

from hrflow_connectors.v2.core.common import Direction
from hrflow_connectors.v2.core.context import MAIN_IMPORT_NAME
from hrflow_connectors.v2.core.hrflow import HrFlowWarehouse
from hrflow_connectors.v2.core.msgspec_pydantic_compat import fields
from hrflow_connectors.v2.core.templates import Templates
from hrflow_connectors.v2.core.utils import get_import_name, reindent_function_source

if t.TYPE_CHECKING:
    from hrflow_connectors.v2.core.connector import Connector, Flow  # pragma: nocover


WORKFLOW_ID_SETTINGS_KEY = "__workflow_id"
INCREMENTAL_SETTINGS_KEY = "__incremental"

WORKFLOW_ACTIVATE_INCREMENTAL = "enable"

WORKFLOW_CONNECTOR_AUTH_SETTINGS_PREFIX = "connector_auth_"
WORKFLOW_HRFLOW_AUTH_SETTINGS_PREFIX = "hrflow_auth_"
WORKFLOW_PULL_PARAMETERS_SETTINGS_PREFIX = "pull_parameters_"
WORKFLOW_PUSH_PARAMETERS_SETTINGS_PREFIX = "push_parameters_"

WORKFLOW_LOGICS_PLACEHOLDER = "# << logics_placeholder >>"
WORKFLOW_FORMAT_PLACEHOLDER = "# << format_placeholder >>"
WORKFLOW_CALLBACK_PLACEHOLDER = "# << callback_placeholder >>"
WORKFLOW_EVENT_PARSER_PLACEHOLDER = "# << event_parser_placeholder >>"

WORKFLOW_DEFAULT_EVENT_PARSER_FUNCTION_NAME = "default_event_parser"
WORKFLOW_USER_EVENT_PARSER_FUNCTION_NAME = "event_parser"


def workflow(
    connector: Connector,
    flow: Flow,
    integration: t.Literal["catch", "pull"],
) -> str:
    connector_aisle = connector.warehouse.get_aisle(flow.entity)
    hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)

    # This is only called with a properly validated connector
    # for which below must be true
    assert connector_aisle is not None
    assert hrflow_aisle is not None

    if flow.direction is Direction.inbound:
        origin_parameters = connector_aisle.parameters("read", flow.mode)
        target_parameters = hrflow_aisle.parameters("write", flow.mode)
    else:
        origin_parameters = hrflow_aisle.parameters("read", flow.mode)
        target_parameters = connector_aisle.parameters("write", flow.mode)

    assert origin_parameters is not None
    assert target_parameters is not None

    default_event_parser = ""
    if flow.event_parser is not None:
        default_event_parser = inspect.getsource(flow.event_parser).replace(
            flow.event_parser.__name__, WORKFLOW_DEFAULT_EVENT_PARSER_FUNCTION_NAME
        )
        default_event_parser = reindent_function_source(
            default_event_parser,
            function_name=WORKFLOW_DEFAULT_EVENT_PARSER_FUNCTION_NAME,
        )

    return Templates.get_template("workflow.py.j2").render(
        main_module=MAIN_IMPORT_NAME.get(),
        import_name=get_import_name(connector),
        workflow_id_settings_key=WORKFLOW_ID_SETTINGS_KEY,
        incremental_settings_key=INCREMENTAL_SETTINGS_KEY,
        activate_incremental_token=WORKFLOW_ACTIVATE_INCREMENTAL,
        connector_auth_settings_prefix=WORKFLOW_CONNECTOR_AUTH_SETTINGS_PREFIX,
        hrflow_auth_settings_prefix=WORKFLOW_HRFLOW_AUTH_SETTINGS_PREFIX,
        pull_parameters_settings_prefix=WORKFLOW_PULL_PARAMETERS_SETTINGS_PREFIX,
        push_parameters_settings_prefix=WORKFLOW_PUSH_PARAMETERS_SETTINGS_PREFIX,
        logics_placeholder=WORKFLOW_LOGICS_PLACEHOLDER,
        format_placeholder=WORKFLOW_FORMAT_PLACEHOLDER,
        callback_placeholder=WORKFLOW_CALLBACK_PLACEHOLDER,
        event_parser_placeholder=WORKFLOW_EVENT_PARSER_PLACEHOLDER,
        default_event_parser=default_event_parser,
        user_event_parser_function_name=WORKFLOW_USER_EVENT_PARSER_FUNCTION_NAME,
        default_event_parser_function_name=WORKFLOW_DEFAULT_EVENT_PARSER_FUNCTION_NAME,
        action_name=flow.name(connector.subtype),
        type=integration,
        connector_auth=fields(connector.warehouse.auth),
        hrflow_auth=fields(HrFlowWarehouse.auth),
        pull_parameters=fields(origin_parameters),
        push_parameters=fields(target_parameters),
    )
