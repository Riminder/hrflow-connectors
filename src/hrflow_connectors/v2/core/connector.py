import json
import typing as t
from dataclasses import dataclass, field
from enum import Enum
from logging import getLogger
from pathlib import Path

from hrflow_connectors.v2.core.common import Direction, Entity, Mode, Schema
from hrflow_connectors.v2.core.hrflow import HrFlowWarehouse
from hrflow_connectors.v2.core.msgspec_pydantic_compat import json_schema
from hrflow_connectors.v2.core.run import (
    ActionInitError,
    CallbackT,
    FormatT,
    LogicsT,
    Metadata,
    RunResult,
    run,
)
from hrflow_connectors.v2.core.templating import WORKFLOW, workflow
from hrflow_connectors.v2.core.utils import CONNECTORS_DIRECTORY, compute_logo_path
from hrflow_connectors.v2.core.warehouse import Aisle, Warehouse

default_logger = getLogger(__name__)


def is_lambda(fn: t.Callable):
    return fn.__name__ == (lambda: None).__name__


EventParserT = t.Callable[[dict], dict]


class NoLambdaEventParser(Exception):
    pass


@dataclass
class Flow:
    mode: Mode
    entity: Entity
    direction: Direction
    override_name: t.Optional[str] = None
    format: t.Optional[FormatT] = None
    logics: t.Optional[LogicsT] = None
    callback: t.Optional[CallbackT] = None
    event_parser: t.Optional[EventParserT] = None

    def __post_init__(self):
        if self.event_parser is not None and is_lambda(self.event_parser):
            raise NoLambdaEventParser(
                "event_parser if supplied should not be a lambda "
                "function: Please use a regular 'def' function"
            )

    def default_name(self, connector_subtype: str):
        return (
            f"{self.mode.value}_{self.entity.name}s"
            f"_in_{'hrflow' if self.direction is Direction.inbound else connector_subtype}"  # noqa E501
        )

    def name(self, connector_subtype: str):
        return self.override_name or self.default_name(connector_subtype)

    def description(self, connector_name: str) -> str:
        mode: Mode = self.mode
        if mode is Mode.create:
            state = "created"
        elif mode is Mode.update:
            state = "updated"
        elif mode is Mode.archive:
            state = "archived"

        if self.direction is Direction.inbound:
            origin = connector_name
            target = "HrFlow"
        else:
            origin = "HrFlow"
            target = connector_name

        return (
            f"Send **{state}** '{self.entity.value}(s)' _from_ {origin} _to_ {target}"
        )


class ConnectorType(Enum):
    ATS = "ATS"
    CRM = "CRM"
    HCM = "HCM"
    Automation = "Automation"
    JobBoard = "Job Board"
    Classifieds = "Classified Ads"
    Other = "Other"


class InvalidFlow(Exception):
    pass


class MetadataVariableKW(t.TypedDict):
    origin_name: str
    target_name: str


class RunVariableKW(t.TypedDict):
    origin: Aisle
    origin_auth: dict
    origin_auth_schema: Schema
    target: Aisle
    target_auth: dict
    target_auth_schema: Schema


class PublicActionInterface(t.Protocol):
    def __call__(
        self,
        *,
        workflow_id: str,
        connector_auth: dict,
        hrflow_auth: dict,
        pull_parameters: dict,
        push_parameters: dict,
        init_error: t.Optional[ActionInitError] = None,
        format: t.Optional[FormatT] = None,
        logics: t.Optional[LogicsT] = None,
        callback: t.Optional[CallbackT] = None,
        persist: bool = True,
        incremental: bool = False,
    ) -> RunResult:
        ...  # pragma: nocover


def make_action(
    *,
    flow: Flow,
    connector_name: str,
    connector_subtype: str,
    hrflow_aisle: Aisle,
    connector_aisle: Aisle,
    connector_auth_schema: Schema,
) -> PublicActionInterface:
    LocalHrflowWarehouse = HrFlowWarehouse
    if flow.direction is Direction.inbound:
        metadata_kw = MetadataVariableKW(
            origin_name=connector_name,
            target_name="hrflow",
        )
    else:
        metadata_kw = MetadataVariableKW(
            origin_name="hrflow",
            target_name=connector_name,
        )

    def action(
        *,
        workflow_id: str,
        connector_auth: dict,
        hrflow_auth: dict,
        pull_parameters: dict,
        push_parameters: dict,
        init_error: t.Optional[ActionInitError] = None,
        format: t.Optional[FormatT] = None,
        logics: t.Optional[LogicsT] = None,
        callback: t.Optional[CallbackT] = None,
        persist: bool = True,
        incremental: bool = False,
    ):
        if flow.direction is Direction.inbound:
            run_kw = RunVariableKW(
                origin=connector_aisle,
                origin_auth=connector_auth,
                origin_auth_schema=connector_auth_schema,
                target=hrflow_aisle,
                target_auth=hrflow_auth,
                target_auth_schema=LocalHrflowWarehouse.auth,
            )
        else:
            run_kw = RunVariableKW(
                origin=hrflow_aisle,
                origin_auth=hrflow_auth,
                origin_auth_schema=LocalHrflowWarehouse.auth,
                target=connector_aisle,
                target_auth=connector_auth,
                target_auth_schema=connector_auth_schema,
            )

        return run(
            workflow_id=workflow_id,
            metadata=Metadata(
                connector_name=connector_name,
                action_name=flow.name(connector_subtype),
                using_default_format=format is None,
                using_default_logics=logics is None,
                **metadata_kw,
            ),
            mode=flow.mode,
            origin_parameters=pull_parameters,
            target_parameters=push_parameters,
            incremental=incremental,
            init_error=init_error,
            format=format if format is not None else flow.format,
            logics=logics if logics is not None else flow.logics,
            callback=callback if callback is not None else flow.callback,
            persist=persist,
            **run_kw,
        )

    return action


class WorkflowManifest(t.TypedDict):
    catch_template: str
    pull_template: str
    settings_keys: dict[str, str]
    placeholders: dict[str, str]
    expected: dict[str, str]


class ActionManifest(t.TypedDict):
    name: str
    data_type: str
    direction: t.Literal["inbound", "outbound"]
    mode: t.Literal["create", "update", "archive"]
    connector_auth_parameters: dict
    hrflow_auth_parameters: dict
    origin: str
    origin_data_schema: dict
    supports_incremental: bool
    pull_parameters: dict
    target: str
    target_data_schema: dict
    push_parameters: dict
    jsonmap: dict
    workflow: WorkflowManifest


class Manifest(t.TypedDict):
    name: str
    type: str
    subtype: str
    logo: str
    actions: list[ActionManifest]


@dataclass
class Connector:
    name: str
    subtype: str
    description: str
    url: str
    type: ConnectorType
    warehouse: Warehouse
    flows: tuple[Flow, ...] = field(default_factory=tuple)

    def __post_init__(self):
        for flow in self.flows:
            connector_aisle = self.warehouse.get_aisle(flow.entity)
            if connector_aisle is None:
                raise InvalidFlow(
                    f"Invalid flow {flow}: Entity={flow.entity} not supported by"
                    f" {self.name} warehouse"
                )

            hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)
            if hrflow_aisle is None:
                raise InvalidFlow(
                    f"Invalid flow {flow}: Entity={flow.entity} not supported by HrFlow"
                    " warehouse"
                )

            if flow.direction is Direction.inbound:
                if connector_aisle.parameters("read", flow.mode) is None:
                    raise InvalidFlow(
                        f"Invalid flow {flow}: {self.name} warehouse is not readable in"
                        f" mode={flow.mode} for Entity={flow.entity}"
                    )
                if hrflow_aisle.parameters("write", flow.mode) is None:
                    raise InvalidFlow(
                        f"Invalid flow {flow}: HrFlow warehouse is not writable in"
                        f" mode={flow.mode} for Entity={flow.entity}"
                    )
            else:
                if hrflow_aisle.parameters("read", flow.mode) is None:
                    raise InvalidFlow(
                        f"Invalid flow {flow}: HrFlow warehouse is not readable in"
                        f" mode={flow.mode} for Entity={flow.entity}"
                    )
                if connector_aisle.parameters("write", flow.mode) is None:
                    raise InvalidFlow(
                        f"Invalid flow {flow}: {self.name} warehouse is not writable in"
                        f" mode={flow.mode} for Entity={flow.entity}"
                    )

            setattr(
                self,
                flow.name(self.subtype),
                make_action(
                    flow=flow,
                    connector_name=self.name,
                    connector_subtype=self.subtype,
                    hrflow_aisle=hrflow_aisle,
                    connector_aisle=connector_aisle,
                    connector_auth_schema=self.warehouse.auth,
                ),
            )

    def manifest(self, connectors_directory: Path = CONNECTORS_DIRECTORY) -> Manifest:
        actions: list[ActionManifest] = []
        manifest = Manifest(
            name=self.name,
            type=self.type.value.upper().replace(" ", ""),
            subtype=self.subtype,
            logo=compute_logo_path(
                name=self.name,
                subtype=self.subtype,
                connectors_directory=connectors_directory,
            ),
            actions=actions,
        )

        for flow in self.flows:
            hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)
            connector_aisle = self.warehouse.get_aisle(flow.entity)

            # This is already validated in Connector.__post_init__
            assert hrflow_aisle is not None
            assert connector_aisle is not None

            jsonmap_path = (
                connectors_directory
                / self.subtype
                / "mappings"
                / "format"
                / "{}.json".format(flow.name(self.subtype))
            )
            try:
                jsonmap = json.loads(jsonmap_path.read_text())
            except FileNotFoundError:
                jsonmap = {}

            if flow.direction is Direction.inbound:
                origin_aisle = connector_aisle
                target_aisle = hrflow_aisle
            else:
                origin_aisle = hrflow_aisle
                target_aisle = connector_aisle

            pull_parameters = origin_aisle.parameters("read", flow.mode)
            push_parameters = target_aisle.parameters("write", flow.mode)

            # This is already validated in Connector.__post_init__
            assert origin_aisle.read is not None
            assert pull_parameters is not None
            assert push_parameters is not None

            action_manifest = ActionManifest(
                name=flow.name(self.subtype),
                data_type=flow.entity.value,
                direction=flow.direction.value,
                mode=flow.mode.value,
                connector_auth_parameters=json_schema(self.warehouse.auth),
                hrflow_auth_parameters=json_schema(HrFlowWarehouse.auth),
                origin=self.name if flow.direction is Direction.inbound else "HrFlow",
                origin_data_schema=json_schema(origin_aisle.schema),
                supports_incremental=origin_aisle.read.supports_incremental,
                pull_parameters=json_schema(pull_parameters),
                target="HrFlow" if flow.direction is Direction.inbound else self.name,
                target_data_schema=json_schema(target_aisle.schema),
                push_parameters=json_schema(push_parameters),
                jsonmap=jsonmap,
                workflow=WorkflowManifest(
                    catch_template=workflow(
                        connector=self, flow=flow, integration="catch"
                    ),
                    pull_template=workflow(
                        connector=self, flow=flow, integration="pull"
                    ),
                    settings_keys=dict(
                        workflow_id=WORKFLOW.WORKFLOW_ID_SETTINGS_KEY,
                        incremental=WORKFLOW.INCREMENTAL_SETTINGS_KEY,
                        connector_auth_prefix=WORKFLOW.CONNECTOR_AUTH_SETTINGS_PREFIX,
                        hrflow_auth_prefix=WORKFLOW.HRFLOW_AUTH_SETTINGS_PREFIX,
                        pull_parameters_prefix=WORKFLOW.PULL_PARAMETERS_SETTINGS_PREFIX,
                        push_parameters_prefix=WORKFLOW.PUSH_PARAMETERS_SETTINGS_PREFIX,
                    ),
                    placeholders=dict(
                        logics=WORKFLOW.LOGICS_PLACEHOLDER,
                        format=WORKFLOW.FORMAT_PLACEHOLDER,
                        callback=WORKFLOW.CALLBACK_PLACEHOLDER,
                        event_parser=WORKFLOW.EVENT_PARSER_PLACEHOLDER,
                    ),
                    expected=dict(
                        activate_incremental=WORKFLOW.ACTIVATE_INCREMENTAL,
                        logics_functions_name=WORKFLOW.LOGICS_FUNCTIONS_NAME,
                        format_functions_name=WORKFLOW.FORMAT_FUNCTION_NAME,
                        callback_functions_name=WORKFLOW.CALLBACK_FUNCTION_NAME,
                        event_parser_function_name=WORKFLOW.USER_EVENT_PARSER_FUNCTION_NAME,  # noqa E501
                    ),
                ),
            )
            actions.append(action_manifest)

        return manifest


def hrflow_connectors_manifest(
    connectors: t.Iterable[Connector],
    directory_path: str = ".",
    connectors_directory: Path = CONNECTORS_DIRECTORY,
) -> None:
    manifest = dict(
        name="HrFlow.ai Connectors",
        connectors=[
            connector.manifest(connectors_directory=connectors_directory)
            for connector in connectors
        ],
    )

    with open("{}/manifest.json".format(directory_path), "w") as f:
        f.write(json.dumps(manifest, indent=2))
