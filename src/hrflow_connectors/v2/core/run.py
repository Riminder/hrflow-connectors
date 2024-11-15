import time
import typing as t
from collections import Counter
from datetime import datetime, timezone
from enum import Enum
from logging import Logger, LoggerAdapter, getLogger
from uuid import uuid4

from msgspec import Struct, field

from hrflow_connectors.core import backend
from hrflow_connectors.v2.core.common import Mode, Parameters, Schema
from hrflow_connectors.v2.core.msgspec_pydantic_compat import ValidationError, serialize
from hrflow_connectors.v2.core.warehouse import Aisle

default_logger = getLogger(__name__)


class LogTag(Struct):
    name: str
    value: str


def get_adapter(tags: t.Sequence[LogTag], logger: t.Optional[Logger] = None):
    formatted_tags = "".join(["[{}={}]".format(tag.name, tag.value) for tag in tags])

    class Adapter(LoggerAdapter):
        def process(self, msg: str, kwargs: t.Any) -> tuple[str, dict]:
            return (
                "{}: {}".format(
                    formatted_tags,
                    msg,
                ),
                kwargs,
            )

    return Adapter(
        logger=logger or default_logger,
        extra=dict(tags={tag.name: tag.value for tag in tags}),
    )


class Event(Enum):
    read_success = "read_success"
    read_failure = "read_failure"
    format_failure = "format_failure"
    logics_discard = "logics_discard"
    logics_failure = "logics_failure"
    write_failure = "write_failure"
    callback_failure = "callback_failure"
    callback_executed = "callback_executed"
    getting_incremental_token_failure = "getting_incremental_token_failure"

    @classmethod
    def empty_counter(cls) -> t.Counter["Event"]:
        return Counter({event: 0 for event in cls})


class Reason(Enum):
    workflow_id_not_found = "workflow_id_not_found"
    origin_is_not_readable = "origin_is_not_readable"
    target_is_not_writable = "target_is_not_writable"
    origin_does_not_support_incremental = "origin_does_not_support_incremental"
    backend_not_configured = "backend_not_configured"
    event_parsing_failure = "event_parsing_failure"
    getting_incremental_token_failure = "getting_incremental_token_failure"
    mode_not_supported_by_origin = "mode_not_supported_by_origin"
    mode_not_supported_by_target = "mode_not_supported_by_target"
    bad_origin_parameters = "bad_origin_parameters"
    bad_target_parameters = "bad_target_parameters"
    format_failure = "format_failure"
    logics_failure = "logics_failure"
    read_failure = "read_failure"
    write_failure = "write_failure"
    none = "none"


class Status(Enum):
    success = "success"
    success_with_failures = "success_with_failures"
    fatal = "fatal"


class ActionInitError(Struct):
    data: dict
    reason: Reason


class RunResult(Struct):
    incremental: bool
    status: Status
    reason: Reason = Reason.none
    events: t.Counter[Event] = field(default_factory=Event.empty_counter)
    incremental_token: t.Optional[str] = None

    @classmethod
    def from_events(
        cls,
        events: t.Counter[Event],
        incremental: bool,
        incremental_token: t.Optional[str] = None,
    ) -> "RunResult":
        read_success = events[Event.read_success]
        read_failures = events[Event.read_failure]
        if read_success == 0 and read_failures == 0:
            return cls(
                status=Status.success,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )
        elif read_success == 0 and read_failures > 0:
            return cls(
                status=Status.fatal,
                reason=Reason.read_failure,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

        logics_failures = events[Event.logics_failure]
        if logics_failures == read_success:
            return cls(
                status=Status.fatal,
                reason=Reason.logics_failure,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

        logics_discard = events[Event.logics_discard]
        format_failures = events[Event.format_failure]
        if format_failures == read_success - logics_failures - logics_discard:
            return cls(
                status=Status.fatal,
                reason=Reason.format_failure,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

        write_failure = events[Event.write_failure]
        if (
            write_failure
            == read_success - logics_discard - logics_failures - format_failures
        ) and write_failure > 0:
            return cls(
                status=Status.fatal,
                reason=Reason.write_failure,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

        has_failures = any(
            events[event] > 0
            for event in [
                Event.read_failure,
                Event.format_failure,
                Event.logics_failure,
                Event.write_failure,
                Event.callback_failure,
            ]
        )
        if has_failures:
            return cls(
                status=Status.success_with_failures,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )
        return cls(
            status=Status.success,
            events=events,
            incremental=incremental,
            incremental_token=incremental_token,
        )


class Metadata(Struct):
    connector_name: str
    origin_name: str
    target_name: str
    action_name: str
    using_default_format: bool
    using_default_logics: bool


FormatT = t.Callable[[dict], dict]
LogicsT = list[t.Callable[[dict], t.Optional[dict]]]


class CallbackT(t.Protocol):
    def __call__(
        self,
        origin_parameters: Parameters,
        target_parameters: Parameters,
        events: Counter[Event],
        items: list[dict],
    ) -> None:
        ...  # pragma: nocover


def run(
    *,
    workflow_id: str,
    metadata: Metadata,
    mode: Mode,
    origin: Aisle,
    origin_auth_schema: Schema,
    origin_auth: dict,
    origin_parameters: dict,
    target: Aisle,
    target_auth_schema: Schema,
    target_auth: dict,
    target_parameters: dict,
    init_error: t.Optional[ActionInitError] = None,
    incremental: bool = False,
    format: t.Optional[FormatT] = None,
    logics: t.Optional[LogicsT] = None,
    callback: t.Optional[CallbackT] = None,
    persist: bool = True,
):
    action_id = uuid4()
    started_at = datetime.now(tz=timezone.utc)
    adapter = get_adapter(
        tags=[
            LogTag(name="workflow_id", value=workflow_id),
            LogTag(name="action_id", value=action_id.hex),
            LogTag(name="connector", value=metadata.connector_name),
            LogTag(name="origin", value=metadata.origin_name),
            LogTag(name="target", value=metadata.target_name),
            LogTag(name="action_name", value=metadata.action_name),
            LogTag(name="started_at", value=started_at.isoformat()),
        ]
    )

    if init_error is not None:
        adapter.error(
            "Failed to parse event with reason={} data={}".format(
                repr(init_error.reason), init_error.data
            )
        )
        return RunResult(
            status=Status.fatal, reason=init_error.reason, incremental=incremental
        )

    if origin.read is None:
        return RunResult(
            status=Status.fatal,
            reason=Reason.origin_is_not_readable,
            incremental=incremental,
        )

    if target.write is None:
        return RunResult(
            status=Status.fatal,
            reason=Reason.target_is_not_writable,
            incremental=incremental,
        )

    origin_parameters_schema = origin.parameters(operation="read", mode=mode)
    if origin_parameters_schema is None:
        return RunResult(
            status=Status.fatal,
            reason=Reason.mode_not_supported_by_origin,
            incremental=incremental,
        )

    target_parameters_schema = target.parameters(operation="write", mode=mode)
    if target_parameters_schema is None:
        return RunResult(
            status=Status.fatal,
            reason=Reason.mode_not_supported_by_target,
            incremental=incremental,
        )

    adapter.info("Starting Action")
    try:
        parsed_origin_auth_parameters = serialize(origin_auth, origin_auth_schema)
    except ValidationError as e:
        adapter.warning(f"Failed to parse origin_auth with errors={e}")
        return RunResult(
            status=Status.fatal,
            reason=Reason.bad_origin_parameters,
            incremental=incremental,
        )

    try:
        parsed_origin_parameters = serialize(
            origin_parameters, origin_parameters_schema
        )
    except ValidationError as e:
        adapter.warning(f"Failed to parse origin_parameters with errors={e}")
        return RunResult(
            status=Status.fatal,
            reason=Reason.bad_origin_parameters,
            incremental=incremental,
        )

    try:
        parsed_target_auth_parameters = serialize(target_auth, target_auth_schema)
    except ValidationError as e:
        adapter.warning(f"Failed to parse target_auth with errors={e}")
        return RunResult(
            status=Status.fatal,
            reason=Reason.bad_target_parameters,
            incremental=incremental,
        )

    try:
        parsed_target_parameters = serialize(
            target_parameters, target_parameters_schema
        )
    except ValidationError as e:
        adapter.warning(f"Failed to parse target_parameters with errors={e}")
        return RunResult(
            status=Status.fatal,
            reason=Reason.bad_target_parameters,
            incremental=incremental,
        )

    incremental_token = None
    if incremental:
        if origin.read.supports_incremental is False:
            adapter.warning(
                f"Origin warehouse {metadata.origin_name} does not support incremetal"
                " reading"
            )
            return RunResult(
                status=Status.fatal,
                reason=Reason.origin_does_not_support_incremental,
                incremental=incremental,
            )

        if backend.store is None:
            adapter.warning("Backend not configured: Cannot run in incremental mode")
            return RunResult(
                status=Status.fatal,
                reason=Reason.backend_not_configured,
                incremental=incremental,
            )

        adapter.info("Reading in incremental mode: fetching last token")
        last_results = backend.store.load(key=workflow_id, parse_as=RunResult)
        incremental_token = (
            last_results.incremental_token if last_results is not None else None
        )

    events = Event.empty_counter()

    read_started_at = time.time()
    adapter.info(
        f"Starting to read from warehouse={metadata.origin_name} with "
        f"mode={mode} incremental={incremental} incremental_token={incremental_token}"
    )
    origin_items = []
    try:
        for item in origin.read(
            mode=mode,
            adapter=adapter,
            auth_parameters=parsed_origin_auth_parameters,
            parameters=parsed_origin_parameters,
            incremental=incremental,
            incremental_token=incremental_token,
        ):
            origin_items.append(item)
            events[Event.read_success] += 1
    except Exception as e:
        events[Event.read_failure] += 1
        adapter.exception(
            f"Failed to read from warehouse={metadata.origin_name} with error={repr(e)}"
        )
    if len(origin_items) == 0:
        if events[Event.read_failure] > 0:
            adapter.warning(
                "No items fetched from origin warehoue. Aborting action after"
                " read_failure"
            )
        return RunResult.from_events(
            events, incremental=incremental, incremental_token=incremental_token
        )

    read_finished_at = time.time()
    adapter.info(
        f"Finished reading in {read_finished_at - read_started_at} from"
        f" warehouse={metadata.origin_name}"
        f" n_items={len(origin_items)} read_failure={events[Event.read_failure]}"
    )

    next_incremental_token = incremental_token
    if len(origin_items) > 0 and incremental:
        last_item = origin_items[-1]

        # We know it's not None because of the check
        # origin.read.supports_incremental
        # Adding these kinds of asserts which are anyway removed
        # in optimized Python bytecode is for type checkers only
        assert origin.read.get_incremental_token is not None

        try:
            next_incremental_token = origin.read.get_incremental_token(last_item)
        except Exception as e:
            events[Event.getting_incremental_token_failure] += 1
            adapter.exception(
                f"Failed to get read_from from warehouse={metadata.origin_name} with"
                f" error={repr(e)}"
            )
            return RunResult(
                status=Status.fatal,
                reason=Reason.getting_incremental_token_failure,
                events=events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

    if logics is None or len(logics) == 0:
        if logics is None:
            adapter.info("No logics supplied: Skipping ...")
        elif len(logics) == 0:
            adapter.info("Empty logics array supplied: Skipping ...")
        selected_items = origin_items
    else:
        adapter.info(
            "Starting to apply logic functions: "
            f"n_items={len(origin_items)} before applying logics"
        )
        selected_items = []
        for item in origin_items:
            for i, logic in enumerate(logics):
                try:
                    item = logic(item)
                except Exception as e:
                    adapter.exception(
                        f"Failed to apply logic function number={i} error={repr(e)}"
                    )
                    events[Event.logics_failure] += 1
                    break
                if item is None:
                    events[Event.logics_discard] += 1
                    break
            else:
                selected_items.append(item)

        if len(selected_items) == 0:
            adapter.warning(
                "Logics failed for all items. Review supplied logic functions."
                " Aborting action."
            )
            return RunResult.from_events(
                events,
                incremental=incremental,
                incremental_token=incremental_token,
            )
        adapter.info(
            "Finished applying logic functions: "
            f"success={len(selected_items)} discarded={events[Event.logics_discard]}"
            f" failures={events[Event.logics_failure]}"
        )

    if format is None:
        adapter.info("No format function supplied: Skipping ...")
        formatted_items = selected_items
    else:
        adapter.info(
            "Starting to format origin items using"
            f" {'default' if metadata.using_default_format else 'user defined'} function"
        )
        formatted_items = []
        for item in selected_items:
            try:
                formatted_items.append(format(item))
            except Exception as e:
                events[Event.format_failure] += 1
                adapter.exception(
                    "Failed to format origin item using"
                    f" {'default' if metadata.using_default_format else 'user defined'}"
                    f" function error={repr(e)}"
                )
        adapter.info(
            "Finished formatting origin items"
            f" success={len(formatted_items)} failures={events[Event.format_failure]}"
        )

        if len(formatted_items) == 0:
            adapter.warning(
                "Formatting failed for all items. Review supplied format function."
                " Aborting action."
            )
            return RunResult.from_events(
                events,
                incremental=incremental,
                incremental_token=incremental_token,
            )

    if persist is False:
        adapter.info(
            f"Running in dry mode with persist={persist}: Ending execution after read,"
            " format and logics"
        )
        return RunResult.from_events(
            events, incremental=incremental, incremental_token=incremental_token
        )

    write_started_at = time.time()
    adapter.info(
        f"Starting to write to warehouse={metadata.target_name} with"
        f" n_items={len(formatted_items)}"
    )
    try:
        failed_items = target.write(
            mode=mode,
            adapter=adapter,
            auth_parameters=parsed_target_auth_parameters,
            parameters=parsed_target_parameters,
            items=formatted_items,
        )
        events[Event.write_failure] += len(failed_items)
    except Exception as e:
        adapter.exception(
            f"Failed to write to warehouse={metadata.target_name} with error={repr(e)}"
        )
        events[Event.write_failure] += len(formatted_items)
        return RunResult(
            status=Status.fatal,
            reason=Reason.write_failure,
            events=events,
            incremental=incremental,
            incremental_token=incremental_token,
        )
    write_finished_at = time.time()
    adapter.info(
        f"Finished writing in {write_finished_at - write_started_at} "
        f"to warehouse={metadata.target_name} "
        f"success={len(formatted_items) - events[Event.write_failure]} "
        f"failures={events[Event.write_failure]}"
    )

    if callback is not None:
        adapter.info("Calling callback function")
        try:
            callback(
                parsed_origin_parameters,
                parsed_target_parameters,
                events,
                formatted_items,
            )
        except Exception as e:
            events[Event.callback_failure] += 1
            adapter.exception(f"Failed to run callback with error={repr(e)}")
        finally:
            events[Event.callback_executed] += 1

    results = RunResult.from_events(
        events, incremental=incremental, incremental_token=next_incremental_token
    )
    if backend.store is not None:
        adapter.info(f"Saving run results in {backend.store.name} backend")
        backend.store.save(key=workflow_id, data=results)

    adapter.info("Finished action")
    return results
