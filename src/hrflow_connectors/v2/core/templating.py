from __future__ import annotations

import inspect
import re
import typing as t
from dataclasses import dataclass

from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.core.common import Direction
from hrflow_connectors.v2.core.context import MAIN_IMPORT_NAME
from hrflow_connectors.v2.core.hrflow import HrFlowWarehouse
from hrflow_connectors.v2.core.msgspec_pydantic_compat import fields, template_fields
from hrflow_connectors.v2.core.run import CallbackT, FormatT, LogicsT
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


class InvalidConnectorReadmeContent(Exception):
    pass


@dataclass(frozen=True)
class CONNECTOR_README:
    ACTIONS_SECTIONS_REGEXP = (
        r"# 🔌 Connector Actions.+?\|\s*Action\s*\|\s*Description\s*\|.+?\|\s+?<\/p>"
    )


def connector_readme(
    connector: Connector, current_content: t.Optional[str] = None
) -> str:
    if current_content is None:
        return Templates.get_template("connector_readme.md.j2").render(
            connector_name=connector.name.capitalize(),
            connector_subtype=connector.subtype,
            description=connector.description,
            url=connector.url,
            flows=connector.flows,
        )
    else:
        match = re.search(
            CONNECTOR_README.ACTIONS_SECTIONS_REGEXP, current_content, re.DOTALL
        )
        if match is None:
            raise InvalidConnectorReadmeContent(
                "README.md for connector {} does not respect standard format. No"
                " actions section found".format(connector.name)
            )
        updated_actions_content = Templates.get_template(
            "connector_actions.md.j2"
        ).render(
            flows=connector.flows,
        )
        return "{before}{actions}{after}".format(
            before=current_content[: match.start()],
            actions=updated_actions_content,
            after=current_content[match.end() :],
        )


@dataclass(frozen=True)
class CONNECTOR_ACTION:
    class OtherFields(Struct):
        workflow_id: Annotated[
            str,
            Meta(
                description=(
                    "A stable identifier used for persisting in incremental mode"
                )
            ),
        ]
        logics: Annotated[
            t.Optional[LogicsT],
            Meta(
                description=(
                    "A list of functions called in sequence with each"
                    " item pulled from the origin. Each function might either "
                    "return it's argument or None to discard the item. Any item"
                    " discarded is eventually not pushed to the target"
                ),
                extra_json_schema=dict(),
            ),
        ]
        format: Annotated[
            t.Optional[FormatT],
            Meta(
                description=(
                    "A formatting function to apply on items pulled before the push"
                ),
                extra_json_schema=dict(),
            ),
        ] = None
        callback: Annotated[
            t.Optional[CallbackT],
            Meta(
                description=(
                    "Registers a callback function to be called at the "
                    "of a successful execution"
                ),
                extra_json_schema=dict(),
            ),
        ] = None
        persist: Annotated[
            bool,
            Meta(
                description=(
                    "When False has the effect of running "
                    "in dry mode. Items are pulled but not pushed to the target"
                )
            ),
        ] = True
        incremental: Annotated[
            bool, Meta(description="Controls the incremental reading execution mode")
        ] = False

    OTHER_FIELDS = OtherFields


def connector_action(connector: Connector, flow: Flow) -> str:
    import_name = get_import_name(connector)
    action_name = flow.name(connector.subtype)
    if flow.direction is Direction.inbound:
        origin_name = connector.name
        target_name = "HrFlow"
    else:
        origin_name = "HrFlow"
        target_name = connector.name

    connector_aisle = connector.warehouse.get_aisle(flow.entity)
    hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)

    # This is only called with a properly validated
    # connector for which below must be true
    assert connector_aisle is not None
    assert hrflow_aisle is not None

    if flow.direction is Direction.inbound:
        origin_aisle = connector_aisle
        target_aisle = hrflow_aisle
    else:
        origin_aisle = hrflow_aisle
        target_aisle = connector_aisle

    # This is only called with a properly validated
    # connector for which below must be true
    assert origin_aisle.read is not None
    assert target_aisle.write is not None

    pull_parameters = origin_aisle.parameters("read", flow.mode)
    push_parameters = target_aisle.parameters("write", flow.mode)

    assert pull_parameters is not None
    assert push_parameters is not None

    return (
        Templates.get_template("action_readme.md.j2")
        .render(
            action_name=action_name,
            origin_name=origin_name,
            target_name=target_name,
            description=flow.description(connector.name),
            origin_endpoints=origin_aisle.read.endpoints.for_mode(flow.mode),
            target_endpoints=target_aisle.write.endpoints.for_mode(flow.mode),
            connector_name=connector.name,
            connector_auth_fields=template_fields(connector.warehouse.auth),
            hrflow_auth_fields=template_fields(HrFlowWarehouse.auth),
            pull_fields=template_fields(pull_parameters),
            push_fields=template_fields(push_parameters),
            other_fields=template_fields(CONNECTOR_ACTION.OTHER_FIELDS),
            main_module=MAIN_IMPORT_NAME.get(),
            import_name=import_name,
        )
        .replace(",\n    \n)", "\n)")
    )


@dataclass(frozen=True)
class CONNECTOR_STUB:
    pass


def connector_stub(
    connector: Connector,
) -> str:
    return Templates.get_template("connector.pyi.j2").render(
        import_name=get_import_name(connector),
        actions=[flow.name(connector.subtype) for flow in connector.flows],
    )
