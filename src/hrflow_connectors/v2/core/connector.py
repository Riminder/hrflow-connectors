import typing as t
from dataclasses import dataclass, field
from enum import Enum
from logging import getLogger

from hrflow_connectors.v2.core.common import Direction, Entity, Mode, Schema
from hrflow_connectors.v2.core.hrflow import HrFlowWarehouse as HrFlowWarehouse
from hrflow_connectors.v2.core.run import (
    ActionInitError,
    CallbackT,
    FormatT,
    LogicsT,
    Metadata,
    RunResult,
    run,
)
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
        return f"{self.mode.value}_{self.entity.name}s_in_{'hrflow' if self.direction is Direction.inbound else connector_subtype}"

    def name(self, connector_subtype: str):
        return self.override_name or self.default_name(connector_subtype)


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
    ) -> RunResult: ...


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
                    f"Invalid flow {flow}: Entity={flow.entity} not supported by {self.name} warehouse"
                )

            hrflow_aisle = HrFlowWarehouse.get_aisle(flow.entity)
            if hrflow_aisle is None:
                raise InvalidFlow(
                    f"Invalid flow {flow}: Entity={flow.entity} not supported by HrFlow warehouse"
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
