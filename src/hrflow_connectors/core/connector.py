from __future__ import annotations

import copy
import enum
import json
import logging
import time
import typing as t
import uuid
import warnings
from collections import Counter
from datetime import datetime
from functools import partial
from pathlib import Path

from pydantic import (
    BaseModel,
    Field,
    ValidationError,
    create_model,
    root_validator,
    validator,
)

from hrflow_connectors.core import backend
from hrflow_connectors.core.templates import Templates
from hrflow_connectors.core.warehouse import ReadMode, Warehouse

HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE = (
    "https://raw.githubusercontent.com/Riminder/hrflow-connectors"
)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"
KB = 1024
MAX_LOGO_SIZE_BYTES = 100 * KB
MAX_LOGO_PIXEL = 150
MIN_LOGO_PIXEL = 34
logger = logging.getLogger(__name__)


class ConnectorActionAdapter(logging.LoggerAdapter):
    def process(self, msg: str, kwargs: t.Dict) -> t.Tuple[str, t.Dict]:
        tags = [
            "[{}={}]".format(tag["name"], tag["value"])
            for tag in self.extra["log_tags"]
        ]
        return (
            "{}: {}".format(
                "".join(tags),
                msg,
            ),
            kwargs,
        )


class Event(str, enum.Enum):
    read_success = "read_success"
    read_failure = "read_failure"
    format_failure = "format_failure"
    logics_discard = "logics_discard"
    logics_failure = "logics_failure"
    write_failure = "write_failure"
    callback_failure = "callback_failure"
    callback_executed = "callback_executed"
    item_to_read_from_failure = "item_to_read_from_failure"

    @classmethod
    def empty_counter(cls) -> t.Counter["Event"]:
        return Counter({event: 0 for event in cls})


class Reason(str, enum.Enum):
    item_to_read_from_failure = "item_to_read_from_failure"
    origin_does_not_support_incremental = "origin_does_not_support_incremental"
    backend_not_configured_in_incremental_mode = (
        "backend_not_configured_in_incremental_mode"
    )
    workflow_id_not_found = "workflow_id_not_found"
    event_parsing_failure = "event_parsing_failure"
    bad_action_parameters = "bad_action_parameters"
    bad_origin_parameters = "bad_origin_parameters"
    bad_target_parameters = "bad_target_parameters"
    format_failure = "format_failure"
    logics_failure = "logics_failure"
    read_failure = "read_failure"
    write_failure = "write_failure"
    none = ""


class Status(str, enum.Enum):
    success = "success"
    success_with_failures = "success_with_failures"
    fatal = "fatal"


class ActionInitError(BaseModel):
    data: t.Dict
    reason: Reason


class RunResult(BaseModel):
    status: Status
    reason: Reason = Reason.none
    events: t.Counter[Event] = Field(default_factory=Event.empty_counter)
    read_from: t.Optional[str] = None

    @classmethod
    def from_events(cls, events: t.Counter[Event]) -> "RunResult":
        read_success = events[Event.read_success]
        read_failures = events[Event.read_failure]
        if read_success == 0 and read_failures == 0:
            return cls(status=Status.success, events=events)
        elif read_success == 0 and read_failures > 0:
            return cls(
                status=Status.fatal,
                reason=Reason.read_failure,
                events=events,
            )

        format_failures = events[Event.format_failure]
        if format_failures == read_success:
            return cls(
                status=Status.fatal,
                reason=Reason.format_failure,
                events=events,
            )

        logics_failures = events[Event.logics_failure]
        if logics_failures == read_success - format_failures:
            return cls(
                status=Status.fatal,
                reason=Reason.logics_failure,
                events=events,
            )

        logics_discard = events[Event.logics_discard]
        write_failure = events[Event.write_failure]
        if (
            write_failure
            == read_success - format_failures - logics_discard - logics_failures
        ) and write_failure > 0:
            return cls(
                status=Status.fatal,
                reason=Reason.write_failure,
                events=events,
            )

        success_with_failures = any(
            events[event] > 0
            for event in [
                Event.read_failure,
                Event.format_failure,
                Event.logics_failure,
                Event.write_failure,
                Event.callback_failure,
            ]
        )
        if success_with_failures:
            return cls(status=Status.success_with_failures, events=events)
        return cls(status=Status.success, events=events)


LogicFunctionType = t.Callable[[t.Dict], t.Union[t.Dict, None]]
# Different versions of Python produce different string
# reprensentations for t.Union[t.Any, None] to avoid
# inconsistencies in manifest this is hardcoded belo
LogicFunctionTypeStr = "typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]"
LogicsTemplate = """
import typing as t

def logic_1(item: t.Dict) -> t.Union[t.Dict, None]:
    return None

def logic_2(item: t.Dict) -> t.Uniont[t.Dict, None]:
    return None

logics = [logic_1, logic_2]
"""
LogicsDescription = "List of logic functions"
FormatFunctionType = t.Callable[[t.Dict], t.Dict]
FormatTemplate = """
import typing as t

def format(item: t.Dict) -> t.Dict:
    return item
"""
FormatDescription = "Formatting function"
EventParserFunctionType = t.Callable[[t.Dict], t.Dict]
EventParserTemplate = """
import typing as t

def event_parser(event: t.Dict) -> t.Dict:
    parsed = dict()
    parsed["user_id"] = event["email"]
    parsed["thread_id"] = event["subscription_id"]
    return parsed
"""
EventParserDescription = "Event parsing function"
EventParserExtra = dict(skip_from_docs=True)


class BaseActionParameters(BaseModel):
    logics: t.List[LogicFunctionType] = Field(
        default_factory=list, description=LogicsDescription
    )
    format: FormatFunctionType = Field(lambda x: x, description=FormatDescription)
    event_parser: t.Optional[EventParserFunctionType] = Field(
        None, description=EventParserDescription, **EventParserExtra
    )
    read_mode: ReadMode = Field(
        ReadMode.sync,
        description=(
            "If 'incremental' then `read_from` of the last run is given to Origin"
            " Warehouse during read. **The actual behavior depends on implementation of"
            " read**. In 'sync' mode `read_from` is neither fetched nor given to Origin"
            " Warehouse during read."
        ),
    )

    class Config:
        extra = "forbid"

        @staticmethod
        def schema_extra(
            schema: t.Dict[str, t.Any], model: t.Type["BaseActionParameters"]
        ) -> None:
            # JSON has no equivalent for Callable type which is used for
            # logics, format and event_parser. Thus we hardcode properties here
            schema["properties"]["logics"] = {
                "title": "logics",
                "description": (
                    "List of logic functions. Each function should have"
                    " the following signature {}. The final list should be exposed "
                    "in a variable named 'logics'.".format(LogicFunctionTypeStr)
                ),
                "template": LogicsTemplate,
                "type": "code_editor",
            }

            schema["properties"]["format"] = {
                "title": "format",
                "description": (
                    "Formatting function. You should expose a function"
                    " named 'format' with following signature {}".format(
                        FormatFunctionType
                    )
                ),
                "template": FormatTemplate,
                "type": "code_editor",
            }

            schema["properties"]["event_parser"] = {
                "title": "event_parser",
                "description": (
                    "Event parsing function for **CATCH** integrations. You should"
                    " expose a function named 'event_parser' with following"
                    " signature {}".format(EventParserFunctionType)
                ),
                "template": EventParserTemplate,
                "type": "code_editor",
            }

    @classmethod
    def with_defaults(
        cls,
        model_name: str,
        *,
        format: t.Optional[FormatFunctionType] = None,
        event_parser: t.Optional[EventParserFunctionType] = None,
    ) -> t.Type["BaseActionParameters"]:
        new_model = cls
        if format is not None:
            new_model = create_model(
                model_name,
                format=(
                    FormatFunctionType,
                    Field(format, description=FormatDescription),
                ),
                __base__=new_model,
            )
        if event_parser is not None:
            new_model = create_model(
                model_name,
                event_parser=(
                    EventParserFunctionType,
                    Field(
                        event_parser,
                        description=EventParserDescription,
                        **EventParserExtra,
                    ),
                ),
                __base__=new_model,
            )
        return new_model


class WorkflowType(str, enum.Enum):
    catch = "hook"
    pull = "schedule"


class ActionName(str, enum.Enum):
    pull_application_list = "pull_application_list"
    pull_job_list = "pull_job_list"
    pull_profile_list = "pull_profile_list"
    pull_resume_attachment_list = "pull_resume_attachment_list"
    push_profile = "push_profile"
    push_job = "push_job"
    push_profile_list = "push_profile_list"
    push_job_list = "push_job_list"
    push_score_list = "push_score_list"
    catch_profile = "catch_profile"
    catch_job = "catch_job"
    # TalentSoft actions
    applicant_new = "applicant_new"
    applicant_resume_update = "applicant_resume_update"
    applicant_update = "applicant_update"


class ActionType(str, enum.Enum):
    """
    ActionType is used to distinguish between inbound and outbound actions.
    Inbound actions are used to fetch data from external sources and push
    it to HrFlow.ai.
    Outbound actions are used to fetch data from HrFlow.ai and push it
    to external sources.
    """

    inbound = "inbound"
    outbound = "outbound"


class ConnectorAction(BaseModel):
    WORKFLOW_FORMAT_PLACEHOLDER = "# << format_placeholder >>"
    WORKFLOW_LOGICS_PLACEHOLDER = "# << logics_placeholder >>"
    WORKFLOW_EVENT_PARSER_PLACEHOLDER = "# << event_parser_placeholder >>"
    ORIGIN_SETTINGS_PREFIX = "origin_"
    TARGET_SETTINGS_PREFIX = "target_"
    WORKFLOW_ID_SETTINGS_KEY = "__workflow_id"
    trigger_type: WorkflowType
    name: ActionName
    description: str
    parameters: t.Type[BaseModel]
    origin: Warehouse
    target: Warehouse
    callback: t.Optional[
        t.Callable[[BaseModel, BaseModel, t.Counter[Event], t.List[t.Dict]], None]
    ] = None
    action_type: ActionType

    @classmethod
    def based_on(
        cls: t.Type[t.Self],
        base: t.Self,
        connector_name: str,
        with_format: t.Optional[FormatFunctionType] = None,
        with_event_parser: t.Optional[EventParserFunctionType] = None,
    ) -> t.Self:
        default_format = base.parameters.__fields__["format"].default
        default_event_parser = base.parameters.__fields__["event_parser"].default
        parameters = BaseActionParameters.with_defaults(
            "{}{}".format(connector_name, base.parameters.__name__),
            format=with_format or default_format,
            event_parser=with_event_parser or default_event_parser,
        )
        return cls(
            name=base.name,
            trigger_type=base.trigger_type,
            description=base.description,
            parameters=parameters,
            origin=base.origin,
            target=base.target,
            callback=base.callback,
            action_type=base.action_type,
        )

    @validator("origin", pre=False)
    def origin_is_readable(cls, origin):
        if origin.is_readable is False:
            raise ValueError("Origin warehouse is not readable")
        return origin

    @validator("target", pre=False)
    def target_is_writable(cls, target):
        if target.is_writable is False:
            raise ValueError("Target warehouse is not writable")
        return target

    @validator("name", pre=False)
    def name_is_coherent_with_trigger_type(cls, v, values, **kwargs):
        if (
            v
            in [
                ActionName.pull_application_list,
                ActionName.pull_job_list,
                ActionName.pull_profile_list,
            ]
            and values["trigger_type"] != WorkflowType.pull
        ):
            raise ValueError(
                "`pull_application_list`, `pull_job_list` and `pull_profile_list`"
                " are only available for"
                " trigger_type={}".format(WorkflowType.pull)
            )
        return v

    @property
    def data_type(self) -> str:
        return self.origin.data_type.name

    def workflow_code(self, connector_name: str, workflow_type: WorkflowType) -> str:
        return Templates.get_template("workflow.py.j2").render(
            format_placeholder=self.WORKFLOW_FORMAT_PLACEHOLDER,
            logics_placeholder=self.WORKFLOW_LOGICS_PLACEHOLDER,
            event_parser_placeholder=self.WORKFLOW_EVENT_PARSER_PLACEHOLDER,
            workflow_id_settings_key=self.WORKFLOW_ID_SETTINGS_KEY,
            origin_settings_prefix=self.ORIGIN_SETTINGS_PREFIX,
            target_settings_prefix=self.TARGET_SETTINGS_PREFIX,
            connector_name=connector_name,
            action_name=self.name.value,
            type=workflow_type.name,
            origin_parameters=[
                parameter for parameter in self.origin.read.parameters.__fields__
            ],
            target_parameters=[
                parameter for parameter in self.target.write.parameters.__fields__
            ],
        )

    def run(
        self,
        connector_name: str,
        workflow_id: str,
        action_parameters: t.Dict,
        origin_parameters: t.Dict,
        target_parameters: t.Dict,
        init_error: t.Optional[ActionInitError] = None,
    ) -> RunResult:
        action_id = uuid.uuid4()
        started_at = datetime.utcnow()
        adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=[
                    dict(name="started_at", value=started_at.isoformat()),
                    dict(name="connector", value=connector_name),
                    dict(name="action_name", value=self.name),
                    dict(name="workflow_id", value=workflow_id),
                    dict(name="action_id", value=action_id),
                ]
            ),
        )

        if init_error is not None:
            adapter.error(
                "Failed to parse event with reason={} data={}".format(
                    repr(init_error.reason), init_error.data
                )
            )
            return RunResult(
                status=Status.fatal,
                reason=init_error.reason,
            )

        adapter.info("Starting Action")
        try:
            parameters = self.parameters(**action_parameters)
        except ValidationError as e:
            adapter.warning(
                "Failed to parse action_parameters with errors={}".format(e.errors())
            )
            return RunResult(status=Status.fatal, reason=Reason.bad_action_parameters)

        try:
            origin_parameters = self.origin.read.parameters(**origin_parameters)
        except ValidationError as e:
            adapter.warning(
                "Failed to parse origin_parameters with errors={}".format(e.errors())
            )
            return RunResult(status=Status.fatal, reason=Reason.bad_origin_parameters)

        try:
            target_parameters = self.target.write.parameters(**target_parameters)
        except ValidationError as e:
            adapter.warning(
                "Failed to parse target_parameters with errors={}".format(e.errors())
            )
            return RunResult(status=Status.fatal, reason=Reason.bad_target_parameters)

        if parameters.read_mode is ReadMode.incremental:
            if self.origin.supports_incremental is False:
                adapter.warning(
                    "Origin warehouse {} does not support '{}' read mode".format(
                        self.origin.name, ReadMode.incremental.value
                    )
                )
                return RunResult(
                    status=Status.fatal,
                    reason=Reason.origin_does_not_support_incremental,
                )

            if backend.is_configured is False:
                adapter.warning(
                    "For '{}' read_mode backend must be configured".format(
                        ReadMode.incremental.value
                    )
                )
                return RunResult(
                    status=Status.fatal,
                    reason=Reason.backend_not_configured_in_incremental_mode,
                )

        read_from = None
        if parameters.read_mode is ReadMode.incremental:
            adapter.info(
                "Read mode is '{}' fetching last run results".format(
                    ReadMode.incremental.value
                )
            )
            last_results = backend.store.load(key=workflow_id, parse_as=RunResult)
            read_from = last_results.read_from if last_results is not None else None

        events = Event.empty_counter()

        read_started_at = time.time()
        adapter.info(
            "Starting to read from warehouse={} with mode={} read_from={} parameters={}"
            .format(
                self.origin.name,
                parameters.read_mode,
                read_from,
                origin_parameters,
            )
        )
        origin_adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=adapter.extra["log_tags"]
                + [
                    dict(name="warehouse", value=self.origin.name),
                    dict(name="action", value="read"),
                ]
            ),
        )
        origin_items = []
        try:
            for item in self.origin.read(
                origin_adapter,
                origin_parameters,
                read_mode=parameters.read_mode,
                read_from=read_from,
            ):
                origin_items.append(item)
                events[Event.read_success] += 1
        except Exception as e:
            events[Event.read_failure] += 1
            adapter.exception(
                "Failed to read from warehouse={} with parameters={} error={}".format(
                    self.origin.name, origin_parameters, repr(e)
                )
            )
        if len(origin_items) == 0:
            if events[Event.read_failure] > 0:
                adapter.warning(
                    "No items fetched from origin warehoue. Aborting action after"
                    " read_failure"
                )
            return RunResult.from_events(events)

        read_finished_at = time.time()
        adapter.info(
            "Finished reading in {} from warehouse={} n_items={} read_failure={}"
            .format(
                read_finished_at - read_started_at,
                self.origin.name,
                len(origin_items),
                events[Event.read_failure] > 0,
            )
        )

        next_read_from = read_from
        if len(origin_items) > 0 and parameters.read_mode is ReadMode.incremental:
            last_item = origin_items[-1]
            try:
                next_read_from = self.origin.item_to_read_from(last_item)
            except Exception as e:
                events[Event.item_to_read_from_failure] += 1
                adapter.exception(
                    "Failed to get read_from from warehouse={} with parameters={}"
                    " item={} error={}".format(
                        self.origin.name, origin_parameters, last_item, repr(e)
                    )
                )
                return RunResult(
                    status=Status.fatal,
                    reason=Reason.item_to_read_from_failure,
                    events=events,
                )

        using_default_format = not bool(action_parameters.get("format"))
        adapter.info(
            "Starting to format origin items using {} function".format(
                "default" if using_default_format else "user defined"
            )
        )
        formatted_items = []
        for item in origin_items:
            try:
                formatted_items.append(parameters.format(item))
            except Exception as e:
                events[Event.format_failure] += 1
                adapter.exception(
                    "Failed to format origin item using {} function error={}".format(
                        "default" if using_default_format else "user defined", repr(e)
                    )
                )
        adapter.info(
            "Finished formatting origin items success={} failures={}".format(
                len(formatted_items), events[Event.format_failure]
            )
        )

        if len(formatted_items) == 0:
            adapter.warning(
                "Formatting failed for all items. Review supplied format function."
                " Aborting action."
            )
            return RunResult.from_events(events)

        if len(parameters.logics) > 0:
            adapter.info(
                "Starting to apply logic functions: "
                "n_items={} before applying logics".format(len(formatted_items))
            )
            items_to_write = []
            for item in formatted_items:
                for i, logic in enumerate(parameters.logics):
                    try:
                        item = logic(item)
                    except Exception as e:
                        adapter.exception(
                            "Failed to apply logic function number={} error={}".format(
                                i, repr(e)
                            )
                        )
                        events[Event.logics_failure] += 1
                        break
                    if item is None:
                        events[Event.logics_discard] += 1
                        break
                else:
                    items_to_write.append(item)

            if len(items_to_write) == 0:
                adapter.warning(
                    "Logics failed for all items. Review supplied logic functions."
                    " Aborting action."
                )
                return RunResult.from_events(events)
            adapter.info(
                "Finished applying logic functions: "
                "success={} discarded={} failures={}".format(
                    len(items_to_write),
                    events[Event.logics_discard],
                    events[Event.logics_failure],
                )
            )
        else:
            adapter.info("No logic functions supplied. Skipping")
            items_to_write = formatted_items

        write_started_at = time.time()
        adapter.info(
            "Starting to write to warehouse={} with parameters={} n_items={}".format(
                self.target.name, target_parameters, len(items_to_write)
            )
        )
        target_adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=adapter.extra["log_tags"]
                + [
                    dict(name="warehouse", value=self.target.name),
                    dict(name="action", value="write"),
                ]
            ),
        )
        try:
            failed_items = self.target.write(
                target_adapter, target_parameters, items_to_write
            )
            events[Event.write_failure] += len(failed_items)
        except Exception as e:
            adapter.exception(
                "Failed to write to warehouse={} with parameters={} error={}".format(
                    self.target.name, target_parameters, repr(e)
                )
            )
            events[Event.write_failure] += len(items_to_write)
            return RunResult(
                status=Status.fatal,
                reason=Reason.write_failure,
                events=events,
            )
        write_finished_at = time.time()
        adapter.info(
            "Finished writing in {} to warehouse={} success={} failures={}".format(
                write_finished_at - write_started_at,
                self.target.name,
                len(items_to_write) - events[Event.write_failure],
                events[Event.write_failure],
            )
        )

        if self.callback is not None:
            adapter.info("Calling callback function")
            try:
                self.callback(
                    origin_parameters, target_parameters, events, items_to_write
                )
            except Exception as e:
                events[Event.callback_failure] += 1
                adapter.exception(
                    "Failed to run callback with error={}".format(repr(e))
                )
            finally:
                events[Event.callback_executed] += 1

        results = RunResult.from_events(events)
        results.read_from = next_read_from
        if backend.is_configured:
            adapter.info("Saving run results in {} backend".format(backend.store.name))
            backend.store.save(key=workflow_id, data=results)

        adapter.info("Finished action")
        return results


class ParametersOverride(BaseModel):
    name: ActionName
    format: t.Optional[FormatFunctionType] = None
    event_parser: t.Optional[EventParserFunctionType] = None

    @root_validator
    def not_empty(cls, values):
        if values.get("format") is None and values.get("event_parser") is None:
            raise ValueError("One of `format` or `event_parser` should not be None")
        return values


class ConnectorType(enum.Enum):
    ATS = "ATS"
    CRM = "CRM"
    HCM = "HCM"
    Automation = "Automation"
    JobBoard = "Job Board"
    Classifieds = "Classified Ads"
    Other = "Other"


class ConnectorModel(BaseModel):
    name: str
    description: str
    url: str
    type: ConnectorType
    actions: t.List[ConnectorAction]

    def logo(self, connectors_directory: Path) -> str:
        try:
            from PIL import Image, UnidentifiedImageError
        except ModuleNotFoundError:  # pragma: no cover
            raise Exception(
                "PIL is not found in current environment. Mind that you need to install"
                " the package with dev dependencies to use manifest utility"
            )
        connector_directory = connectors_directory / self.name.lower()
        if not connector_directory.is_dir():
            raise ValueError(
                "No directory found for connector {} in {}".format(
                    self.name, connector_directory
                )
            )
        logo_paths = list(connector_directory.glob("logo.*"))
        if len(logo_paths) == 0:
            raise ValueError(
                "Missing logo for connector {}. Add a logo file at {} named"
                " 'logo.(png|jpeg|...)'".format(self.name, connector_directory)
            )
        elif len(logo_paths) > 1:
            raise ValueError(
                "Found multiple logos for connector {} => {}. Only a single one should"
                " be present".format(self.name, logo_paths)
            )
        logo = logo_paths[0]
        size = logo.lstat().st_size
        if size > MAX_LOGO_SIZE_BYTES:
            raise ValueError(
                "Logo size {} KB for connector {} is above maximum limit of {} KB"
                .format(size // KB, self.name, MAX_LOGO_SIZE_BYTES // KB)
            )
        try:
            width, height = Image.open(logo).size
        except UnidentifiedImageError:
            raise ValueError(
                "Logo file for connector {} at {} doesn't seem to be a valid image"
                .format(self.name, logo)
            )

        if width != height or width > MAX_LOGO_PIXEL or width < MIN_LOGO_PIXEL:
            raise ValueError(
                "Bad logo dimensions of ({}, {}) for connector {}. Logo should have"
                " square dimensions within range {min}x{min} {max}x{max}".format(
                    width,
                    height,
                    self.name,
                    min=MIN_LOGO_PIXEL,
                    max=MAX_LOGO_PIXEL,
                )
            )
        return "{}/master/src/{}".format(
            HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE,
            str(logo).split("src/")[1],
        )

    def action_by_name(self, action_name: str) -> t.Optional[ConnectorAction]:
        if "__actions_by_name" not in self.__dict__:
            self.__dict__["__actions_by_name"] = {
                action.name.value: action for action in self.actions
            }
        return self.__dict__["__actions_by_name"].get(action_name)


class Connector:
    def __init__(self, *args, **kwargs) -> None:
        self.model = ConnectorModel(*args, **kwargs)
        for action in self.model.actions:
            with_connector_name = partial(action.run, connector_name=self.model.name)
            setattr(self, action.name.value, with_connector_name)

    @classmethod
    def based_on(
        cls: t.Type[t.Self],
        base: t.Self,
        name: str,
        type: ConnectorType,
        description: str,
        url: str,
        with_parameters_override: t.Optional[t.List[ParametersOverride]] = None,
        with_actions: t.Optional[t.List[ConnectorAction]] = None,
    ) -> t.Self:
        base_actions = base.model.actions

        with_parameters_override = with_parameters_override or []
        with_actions = with_actions or []

        for parameters_override in with_parameters_override:
            base_action = next(
                (
                    action
                    for action in base_actions
                    if action.name is parameters_override.name
                ),
                None,
            )
            if base_action is None:
                raise ValueError(
                    "Base connector does not have a {} action to override".format(
                        parameters_override.name.name
                    )
                )
            duplicate = next(
                (
                    action
                    for action in with_actions
                    if action.name is parameters_override.name
                ),
                None,
            )
            if duplicate is not None:
                raise ValueError(
                    "Duplicate action name {} in `with_parameters_override` and"
                    " `with_actions`".format(parameters_override.name.name)
                )
            with_actions.append(
                ConnectorAction.based_on(
                    base=base_action,
                    connector_name=name,
                    with_format=parameters_override.format,
                    with_event_parser=parameters_override.event_parser,
                )
            )

        actions = {action.name: action for action in base_actions + with_actions}
        connector = cls(
            name=name,
            type=type,
            description=description,
            url=url,
            actions=list(actions.values()),
        )
        return connector

    def manifest(self, connectors_directory: Path) -> t.Dict:
        model = self.model
        manifest = dict(
            name=model.name,
            actions=[],
            type=model.type.value,
            logo=model.logo(connectors_directory=connectors_directory),
        )
        for action in model.actions:
            format_placeholder = action.WORKFLOW_FORMAT_PLACEHOLDER
            logics_placeholder = action.WORKFLOW_LOGICS_PLACEHOLDER
            event_parser_placeholder = action.WORKFLOW_EVENT_PARSER_PLACEHOLDER
            action_manifest = dict(
                name=action.name.value,
                action_type=action.action_type.value,
                action_parameters=copy.deepcopy(action.parameters.schema()),
                data_type=action.data_type,
                trigger_type=action.trigger_type.value,
                origin=action.origin.name,
                origin_parameters=action.origin.read.parameters.schema(),
                origin_data_schema=action.origin.data_schema.schema(),
                supports_incremental=action.origin.supports_incremental,
                target=action.target.name,
                target_parameters=action.target.write.parameters.schema(),
                target_data_schema=action.target.data_schema.schema(),
                workflow_code=action.workflow_code(
                    connector_name=model.name, workflow_type=action.trigger_type
                ),
                workflow_code_format_placeholder=format_placeholder,
                workflow_code_logics_placeholder=logics_placeholder,
                workflow_code_event_parser_placeholder=event_parser_placeholder,
                workflow_code_workflow_id_settings_key=action.WORKFLOW_ID_SETTINGS_KEY,
                workflow_code_origin_settings_prefix=action.ORIGIN_SETTINGS_PREFIX,
                workflow_code_target_settings_prefix=action.TARGET_SETTINGS_PREFIX,
            )
            if action.trigger_type is WorkflowType.pull:
                action_manifest.pop("workflow_code_event_parser_placeholder")
                action_manifest["action_parameters"]["properties"].pop("event_parser")

            manifest["actions"].append(action_manifest)
        return manifest


def hrflow_connectors_manifest(
    connectors: t.List[Connector],
    directory_path: str = ".",
    connectors_directory: Path = CONNECTORS_DIRECTORY,
) -> None:
    with warnings.catch_warnings():
        warnings.filterwarnings(
            action="ignore",
            message="Callable (_logics|format|event_parser) was excluded",
            category=UserWarning,
        )
        manifest = dict(
            name="HrFlow.ai Connectors",
            connectors=[
                connector.manifest(connectors_directory=connectors_directory)
                for connector in connectors
            ],
        )
    with open("{}/manifest.json".format(directory_path), "w") as f:
        f.write(json.dumps(manifest, indent=2))
