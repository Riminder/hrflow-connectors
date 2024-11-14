import inspect
import json
import typing as t
from pathlib import Path
from unittest import mock

import pytest

from hrflow_connectors import v2
from hrflow_connectors.v2.core import templating
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Flow, WorkflowManifest
from hrflow_connectors.v2.core.run import Event, Reason, RunResult, Status
from tests.v2.core.conftest import (
    SmartLeadsProto,
    TypedSmartLeads,
    smartleads_lead_to_hrflow_job,
)
from tests.v2.core.src.hrflow_connectors.connectors.smartleads.aisles.leads import (
    LEADS_DB,
    LeadsAisle,
)
from tests.v2.core.src.hrflow_connectors.core.hrflow_mini.aisles.jobs import (
    JOBS_DB,
    JobsAisle,
)
from tests.v2.core.utils import added_connectors, random_workflow_id


@pytest.fixture
def globals_with_smartleads(SmartLeads: TypedSmartLeads):
    with mock.patch.object(
        LeadsAisle.read, "function", return_value=LEADS_DB
    ), mock.patch.object(JobsAisle.read, "function", return_value=JOBS_DB):
        setattr(v2, "SmartLeads", SmartLeads)
        yield dict(hrflow_connectors=dict(v2=v2))
    delattr(v2, "SmartLeads")


GetWorkflowManifest = t.Callable[[Flow], WorkflowManifest]


@pytest.fixture
def get_workflow_manifest(
    SmartLeadsF: SmartLeadsProto, connectors_directory: Path
) -> GetWorkflowManifest:
    def _get_workflow(flow: Flow):
        SmartLeads = SmartLeadsF(flows=(flow,))
        with added_connectors(
            [("SmartLeads", SmartLeads)],
        ):
            manifest = SmartLeads.manifest(connectors_directory)
        return next(
            (
                action
                for action in manifest["actions"]
                if action["name"] == flow.name(SmartLeads.subtype)
            ),
        )["workflow"]

    return _get_workflow


GetWorkflowCode = t.Callable[[Flow, t.Literal["catch", "pull"]], str]


@pytest.fixture
def get_workflow_code(get_workflow_manifest: GetWorkflowManifest) -> GetWorkflowCode:
    def _get_workflow_code(flow: Flow, integration: t.Literal["catch", "pull"]):
        if integration == "catch":
            return get_workflow_manifest(flow)["catch_template"]
        return get_workflow_manifest(flow)["pull_template"]

    return _get_workflow_code


GetSettings = t.Callable[[Flow], dict]


@pytest.fixture
def get_settings(get_workflow_manifest: GetWorkflowManifest) -> GetSettings:
    def _get_settings(flow: Flow):
        if flow.entity is not Entity.job:
            raise Exception(
                f"Below configuration only expected to work with {Entity.job}"
            )

        workflow_manifest = get_workflow_manifest(flow)

        common: dict = {
            workflow_manifest["settings_keys"]["workflow_id"]: random_workflow_id(),
            f"{workflow_manifest['settings_keys']['connector_auth_prefix']}smart_tag": (
                "smart::tag::smart"
            ),
            f"{workflow_manifest['settings_keys']['hrflow_auth_prefix']}api_key": (
                "hrflow::hrflower::hrflow"
            ),
        }
        if flow.direction is Direction.inbound:
            common[
                f"{workflow_manifest['settings_keys']['pull_parameters_prefix']}city"
            ] = "Casablanca"
            common[
                f"{workflow_manifest['settings_keys']['push_parameters_prefix']}board_key"
            ] = "new_board"
        else:
            common[
                f"{workflow_manifest['settings_keys']['pull_parameters_prefix']}city"
            ] = "Casablanca"
            common[
                f"{workflow_manifest['settings_keys']['push_parameters_prefix']}"
                "force_candidate_count_zero"
            ] = True
        return common

    return _get_settings


@pytest.mark.parametrize(
    "flow, expected_read",
    [
        (Flow(Mode.create, Entity.job, Direction.inbound), LEADS_DB),
        (Flow(Mode.create, Entity.job, Direction.outbound), JOBS_DB),
    ],
)
def test_pull_workflow_code_works_as_expected_no_configuration(
    get_workflow_code: GetWorkflowCode,
    flow: Flow,
    expected_read: list,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    workflow_code = get_workflow_code(flow, "pull")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code}

run_result = workflow(settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(expected_read)
    assert result.incremental is False


@pytest.mark.parametrize(
    "flow, expected_read",
    [
        (Flow(Mode.create, Entity.job, Direction.inbound), LEADS_DB),
        (Flow(Mode.create, Entity.job, Direction.outbound), JOBS_DB),
    ],
)
def test_catch_workflow_code_works_as_expected_no_configuration(
    get_workflow_code: GetWorkflowCode,
    flow: Flow,
    expected_read: list,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    workflow_code = get_workflow_code(flow, "catch")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code}

run_result = workflow(_request=dict(), settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(expected_read)
    assert result.incremental is False


def test_logics_placeholder_working_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    logics_code = f"""
import random

LOGICS_EXECUTION_COUNT = 0
LOGICS_DISCARD_COUNT = 0

def logics(item: dict) -> dict:
    global LOGICS_EXECUTION_COUNT, LOGICS_DISCARD_COUNT

    LOGICS_EXECUTION_COUNT += 1

    if random.random() > .5 or LOGICS_DISCARD_COUNT == 0:
        LOGICS_DISCARD_COUNT += 1
        return

    return item

{workflow_manifest["expected"]["logics_functions_name"]} = [logics]
"""

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code.replace(workflow_manifest["placeholders"]["logics"], logics_code)}

run_result = workflow(settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)

    logics_execution_count = globals_with_smartleads["LOGICS_EXECUTION_COUNT"]

    assert logics_execution_count == len(LEADS_DB)

    logics_discards = globals_with_smartleads["LOGICS_DISCARD_COUNT"]

    assert logics_discards > 0
    assert result.events[Event.logics_discard] == logics_discards


def test_format_placeholder_working_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    logics_code = f"""
FORMAT_EXECUTION_COUNT = 0

{inspect.getsource(smartleads_lead_to_hrflow_job)}

def {workflow_manifest["expected"]["format_functions_name"]}(item: dict) -> dict:
    global FORMAT_EXECUTION_COUNT

    FORMAT_EXECUTION_COUNT += 1
    return smartleads_lead_to_hrflow_job(item)
"""

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code.replace(workflow_manifest["placeholders"]["format"], logics_code)}

run_result = workflow(settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)

    format_execution_count = globals_with_smartleads["FORMAT_EXECUTION_COUNT"]

    assert format_execution_count == len(LEADS_DB)


def test_callback_placeholder_working_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    callback_code = f"""
CALLBACK_EXECUTED = False

def {workflow_manifest["expected"]["callback_functions_name"]}(*args, **kwargs):
    global CALLBACK_EXECUTED

    CALLBACK_EXECUTED = True
"""

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code.replace(workflow_manifest["placeholders"]["callback"], callback_code)}

run_result = workflow(settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.callback_executed] == 1
    assert result.events[Event.callback_failure] == 0

    assert globals_with_smartleads["CALLBACK_EXECUTED"] is True


def test_default_event_parser_is_working_as_expected(
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    def xxx_yyy_zzz(body: dict):
        # See in script below for where EVENT_PARSER_EXECUTED
        # is defined
        global EVENT_PARSER_EXECUTED, EVENT_PARSED_CALLED_WITH
        EVENT_PARSER_EXECUTED = True
        EVENT_PARSED_CALLED_WITH = {**body}

        return body

    flow = Flow(Mode.create, Entity.job, Direction.inbound, event_parser=xxx_yyy_zzz)

    workflow_code = get_workflow_code(flow, "catch")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

EVENT_PARSER_EXECUTED = False
EVENT_PARSED_CALLED_WITH = None

{workflow_code}

run_result = workflow(_request=dict(secret="very::secret"), settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)

    assert globals_with_smartleads["EVENT_PARSER_EXECUTED"] is True
    assert globals_with_smartleads["EVENT_PARSED_CALLED_WITH"] == dict(
        secret="very::secret"
    )


def test_user_supplied_event_parser_is_working_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    def xxx_yyy_zzz(body: dict):
        # See in script below for where DEFAULT_EVENT_PARSER_EXECUTED
        # is defined
        global DEFAULT_EVENT_PARSER_EXECUTED
        DEFAULT_EVENT_PARSER_EXECUTED = True

        return body

    flow = Flow(Mode.create, Entity.job, Direction.inbound, event_parser=xxx_yyy_zzz)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "catch")

    event_parser_code = f"""
USER_EVENT_PARSER_EXECUTED = False
USER_EVENT_PARSED_CALLED_WITH = None

def {workflow_manifest["expected"]["event_parser_function_name"]}(body: dict) -> dict:
    global USER_EVENT_PARSER_EXECUTED, USER_EVENT_PARSED_CALLED_WITH
    USER_EVENT_PARSER_EXECUTED = True
    USER_EVENT_PARSED_CALLED_WITH = {{**body}}

    return body
"""

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

DEFAULT_EVENT_PARSER_EXECUTED = False

{workflow_code.replace(workflow_manifest["placeholders"]["event_parser"], event_parser_code)}

run_result = workflow(_request=dict(secret="very::secret"), settings=SETTINGS)
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)

    assert globals_with_smartleads["DEFAULT_EVENT_PARSER_EXECUTED"] is False
    assert globals_with_smartleads["USER_EVENT_PARSER_EXECUTED"] is True
    assert globals_with_smartleads["USER_EVENT_PARSED_CALLED_WITH"] == dict(
        secret="very::secret"
    )


def test_request_is_used_for_parameters_in_catch_mode(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "catch")

    # First execution to check that if fails since settings is empty this time
    script = f"""
import json

{workflow_code}

run_result = workflow(_request=dict(), settings=dict({workflow_manifest["settings_keys"]["workflow_id"]}="{random_workflow_id()}"))
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.bad_origin_parameters

    # Second execution the content of settings is inserted in _request
    script = f"""
import json

{workflow_code}

run_result = workflow(_request=json.loads('{json.dumps(get_settings(flow))}'), settings=dict({workflow_manifest["settings_keys"]["workflow_id"]}="{random_workflow_id()}"))
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)


def test_default_parsed_event_return_is_used_for_parameters_in_catch_mode(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    def xxx_yyy_zzz(body: dict):
        return globals()["COMING_FROM_DEFAULT_EVENT_PARSER"]

    flow = Flow(Mode.create, Entity.job, Direction.inbound, event_parser=xxx_yyy_zzz)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "catch")

    script = f"""
import json

COMING_FROM_DEFAULT_EVENT_PARSER = json.loads('{json.dumps(get_settings(flow))}')

{workflow_code}

run_result = workflow(_request=dict(), settings=dict({workflow_manifest["settings_keys"]["workflow_id"]}="{random_workflow_id()}"))
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)


def test_parsed_event_return_is_used_for_parameters_in_catch_mode(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    def xxx_yyy_zzz(body: dict):
        return dict()

    flow = Flow(Mode.create, Entity.job, Direction.inbound, event_parser=xxx_yyy_zzz)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "catch")

    event_parser_code = f"""
import json

def {workflow_manifest["expected"]["event_parser_function_name"]}(body: dict) -> dict:
    global USER_EVENT_PARSER_EXECUTED, USER_EVENT_PARSED_CALLED_WITH
    USER_EVENT_PARSER_EXECUTED = True
    USER_EVENT_PARSED_CALLED_WITH = {{**body}}

    return json.loads('{json.dumps(get_settings(flow))}')
"""

    script = f"""
{workflow_code.replace(workflow_manifest["placeholders"]["event_parser"], event_parser_code)}

run_result = workflow(_request=dict(), settings=dict({workflow_manifest["settings_keys"]["workflow_id"]}="{random_workflow_id()}"))
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)


def test_missing_workflow_id_fails_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

# This remove workflow_id
SETTINGS.pop("{workflow_manifest["settings_keys"]["workflow_id"]}")

DEFAULT_EVENT_PARSER_EXECUTED = False

{workflow_code}

run_result = workflow(settings=SETTINGS)
"""

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.workflow_id_not_found


def test_incremental_works_as_expected(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

SETTINGS["{workflow_manifest["settings_keys"]["incremental"]}"] = "{workflow_manifest["expected"]["activate_incremental"]}"

DEFAULT_EVENT_PARSER_EXECUTED = False

{workflow_code}

run_result = workflow(settings=SETTINGS)
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none
    assert result.incremental is True


def test_incremental_not_activated_if_different_from_expected_token(
    get_workflow_manifest: GetWorkflowManifest,
    get_workflow_code: GetWorkflowCode,
    globals_with_smartleads: dict,
    get_settings: GetSettings,
):
    flow = Flow(Mode.create, Entity.job, Direction.inbound)

    workflow_manifest = get_workflow_manifest(flow)
    workflow_code = get_workflow_code(flow, "pull")

    script = f"""
import json

SETTINGS = json.loads('{json.dumps(get_settings(flow))}')

# incremental only activated if the value matches exactly
SETTINGS["{workflow_manifest["settings_keys"]["incremental"]}"] = "{workflow_manifest["expected"]["activate_incremental"]}" + "xxx"

DEFAULT_EVENT_PARSER_EXECUTED = False

{workflow_code}

run_result = workflow(settings=SETTINGS)
"""  # noqa : E501

    exec(script, globals_with_smartleads)

    result = globals_with_smartleads["run_result"]

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none
    assert result.incremental is False


def test_connector_readme_does_not_fail(SmartLeads: TypedSmartLeads):
    connector_readme = templating.connector_readme(SmartLeads)

    assert connector_readme is not None
    assert isinstance(connector_readme, str)
    assert len(connector_readme) > 10


def test_connector_readme_with_current_content_does_not_fail(
    SmartLeads: TypedSmartLeads,
):
    connector_readme = templating.connector_readme(
        SmartLeads, current_content=templating.connector_readme(SmartLeads)
    )

    assert connector_readme is not None
    assert isinstance(connector_readme, str)
    assert len(connector_readme) > 10


def test_connector_readme_with_current_content_fails_for_invalid_content(
    SmartLeads: TypedSmartLeads,
):
    with pytest.raises(templating.InvalidConnectorReadmeContent):
        templating.connector_readme(
            SmartLeads,
            current_content="""
This is not a valid connector readme
""",
        )


def test_connector_action_does_not_fail(SmartLeads: TypedSmartLeads):
    for flow in SmartLeads.flows:
        with added_connectors(
            [("SmartLeads", SmartLeads)],
        ):
            action_readme = templating.connector_action(SmartLeads, flow=flow)

        assert action_readme is not None
        assert isinstance(action_readme, str)
        assert len(action_readme) > 10
