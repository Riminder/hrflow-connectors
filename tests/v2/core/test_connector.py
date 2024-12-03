from unittest import mock

import pytest

from hrflow_connectors.core import backend
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Flow, InvalidFlow, NoLambdaEventParser
from hrflow_connectors.v2.core.run import (
    ActionInitError,
    Event,
    Reason,
    RunResult,
    Status,
)
from tests.v2.core.conftest import (
    SmartLeadsProto,
    TypedSmartLeads,
    smartleads_lead_to_hrflow_job,
)
from tests.v2.core.src.hrflow_connectors.connectors.smartleads.aisles.leads import (
    LEADS_DB,
    LeadsAisle,
)
from tests.v2.core.src.hrflow_connectors.core.hrflow_mini.aisles.jobs import JobsAisle
from tests.v2.core.utils import random_workflow_id


def test_flow_event_parser_cannot_be_lambda():
    with pytest.raises(NoLambdaEventParser):
        Flow(Mode.create, Entity.job, Direction.inbound, event_parser=lambda body: body)

    my_lambda = lambda body: body  # noqa: E731
    with pytest.raises(NoLambdaEventParser):
        Flow(Mode.create, Entity.job, Direction.inbound, event_parser=my_lambda)

    def regular_def_function(body: dict):
        return body

    Flow(Mode.create, Entity.job, Direction.inbound, event_parser=regular_def_function)


@pytest.mark.parametrize(
    "flows, expected_actions, should_not_be_present",
    [
        (
            (
                Flow(Mode.create, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.inbound),
                Flow(Mode.archive, Entity.job, Direction.outbound),
            ),
            [
                "create_jobs_in_hrflow",
                "update_jobs_in_hrflow",
                "archive_jobs_in_smartleads",
            ],
            [
                "archive_jobs_in_hrflow",
                Flow(Mode.archive, Entity.job, Direction.inbound).name("smartleads"),
                "create_jobs_in_smartleads",
                Flow(Mode.create, Entity.job, Direction.outbound).name("smartleads"),
                "update_jobs_in_smartleads",
                Flow(Mode.update, Entity.job, Direction.outbound).name("smartleads"),
                "archive_jobs_in_hrflow",
                Flow(Mode.archive, Entity.job, Direction.inbound).name("smartleads"),
            ],
        ),
        (
            (
                Flow(Mode.create, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.outbound),
                Flow(Mode.archive, Entity.job, Direction.outbound),
            ),
            [
                "create_jobs_in_hrflow",
                "update_jobs_in_hrflow",
                "update_jobs_in_smartleads",
                "archive_jobs_in_smartleads",
            ],
            [
                "create_jobs_in_smartleads",
                Flow(Mode.create, Entity.job, Direction.outbound).name("smartleads"),
                "archive_jobs_in_hrflow",
                Flow(Mode.archive, Entity.job, Direction.inbound).name("smartleads"),
            ],
        ),
        (
            (
                Flow(
                    Mode.create,
                    Entity.job,
                    Direction.inbound,
                    override_name="xxx_yyy_zzz",
                ),
                Flow(Mode.update, Entity.job, Direction.inbound),
            ),
            ["xxx_yyy_zzz", "update_jobs_in_hrflow"],
            [
                "create_jobs_in_hrflow",
                Flow(Mode.create, Entity.job, Direction.inbound).name("smartleads"),
            ],
        ),
    ],
)
def test_connector_actions_are_set(
    SmartLeadsF: SmartLeadsProto,
    flows: tuple[Flow, ...],
    expected_actions: list[str],
    should_not_be_present: list[str],
):
    SmartLeads = SmartLeadsF(flows=flows)

    for action in expected_actions:
        assert callable(getattr(SmartLeads, action)) is True

    for action in should_not_be_present:
        assert getattr(SmartLeads, action, None) is None


@pytest.mark.parametrize(
    "flows, error_message",
    [
        (
            (
                Flow(Mode.create, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.inbound),
                # Bad
                Flow(Mode.create, Entity.profile, Direction.inbound),
            ),
            f"{Entity.profile} not supported by HrFlow warehouse",
        ),
        (
            (
                Flow(Mode.create, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.inbound),
                Flow(Mode.update, Entity.job, Direction.outbound),
                # Bad
                Flow(Mode.create, Entity.application, Direction.outbound),
                Flow(Mode.archive, Entity.job, Direction.outbound),
            ),
            f"{Entity.application} not supported by SmartLeads warehouse",
        ),
        (
            (
                Flow(
                    Mode.archive,
                    Entity.profile,
                    Direction.inbound,
                    override_name="xxx_yyy_zzz",
                ),
                Flow(Mode.update, Entity.job, Direction.inbound),
            ),
            f"{Entity.profile} not supported by HrFlow warehouse",
        ),
    ],
)
def test_invalid_flow_is_raised(
    SmartLeadsF: SmartLeadsProto, flows: tuple[Flow, ...], error_message: str
):
    with pytest.raises(InvalidFlow) as excinfo:
        SmartLeadsF(flows=flows)

    assert error_message in excinfo.value.args[0]


def test_connector_with_invalid_flow_fails_as_expected(SmartLeadsF: SmartLeadsProto):
    assert LeadsAisle.read is not None
    assert LeadsAisle.write is not None

    assert JobsAisle.read is not None
    assert JobsAisle.write is not None

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(LeadsAisle, "read", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.inbound),))
    assert (
        f"SmartLeads warehouse is not readable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(LeadsAisle.read.criterias, "create", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.inbound),))
    assert (
        f"SmartLeads warehouse is not readable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(JobsAisle, "write", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.inbound),))
    assert (
        f"HrFlow warehouse is not writable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(JobsAisle.write.criterias, "create", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.inbound),))
    assert (
        f"HrFlow warehouse is not writable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(JobsAisle, "read", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.outbound),))

    assert (
        f"HrFlow warehouse is not readable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(JobsAisle.read.criterias, "create", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.outbound),))

    assert (
        f"HrFlow warehouse is not readable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(LeadsAisle, "write", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.outbound),))

    assert (
        f"SmartLeads warehouse is not writable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )

    with pytest.raises(InvalidFlow) as excinfo:
        with mock.patch.object(LeadsAisle.write.criterias, "create", None):
            SmartLeadsF(flows=(Flow(Mode.create, Entity.job, Direction.outbound),))

    assert (
        f"SmartLeads warehouse is not writable in mode={Mode.create} for"
        f" Entity={Entity.job}"
        in excinfo.value.args[0]
    )


@pytest.mark.parametrize(
    "action, connector_auth, hrflow_auth, pull_parameters, push_parameters",
    [
        (
            "create_jobs_in_hrflow",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(city="Casablanca"),
            dict(board_key="new_board"),
        ),
        (
            "update_jobs_in_hrflow",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(category="Corporate"),
            dict(),
        ),
        (
            "archive_jobs_in_hrflow",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(remote=True),
            dict(board_key="old_board"),
        ),
        (
            "create_jobs_in_smartleads",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(city="Casablanca"),
            dict(force_candidate_count_zero=True),
        ),
        (
            "update_jobs_in_smartleads",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(remote=False),
            dict(),
        ),
        (
            "archive_jobs_in_smartleads",
            dict(smart_tag="smart::tag::smart"),
            dict(api_key="hrflow::hrflower::hrflow"),
            dict(remote=True),
            dict(reset_candidate_count=True),
        ),
    ],
)
def test_actions(
    SmartLeads: TypedSmartLeads,
    action: str,
    connector_auth: dict,
    hrflow_auth: dict,
    pull_parameters: dict,
    push_parameters: dict,
):
    result = getattr(SmartLeads, action)(
        workflow_id=f"testing_{action}",
        connector_auth=connector_auth,
        hrflow_auth=hrflow_auth,
        pull_parameters=pull_parameters,
        push_parameters=push_parameters,
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.success

    assert result.events[Event.read_success] > 0
    assert result.events[Event.read_failure] == 0

    assert result.events[Event.write_failure] == 0

    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0

    assert result.events[Event.format_failure] == 0

    assert result.events[Event.callback_executed] == 0
    assert result.events[Event.getting_incremental_token_failure] == 0

    assert result.incremental is False
    assert result.incremental_token is None


# <<< BELOW WE TEST VARIOUS WAYS A FATAL OUTCOME MIGHT HAPPEN >>>


def test_outcome_on_init_error(
    SmartLeads: TypedSmartLeads,
):
    init_error = ActionInitError(
        data=dict(error=True), reason=Reason.workflow_id_not_found
    )

    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id="",
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        push_parameters=dict(board_key="new_board"),
        init_error=init_error,
        incremental=True,
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is init_error.reason


def test_outcome_if_origin_is_not_readable(
    SmartLeads: TypedSmartLeads,
):
    with mock.patch.object(LeadsAisle, "read", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.origin_is_not_readable


def test_outcome_if_target_is_not_writable(
    SmartLeads: TypedSmartLeads,
):
    with mock.patch.object(JobsAisle, "write", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.target_is_not_writable


def test_outcome_if_mode_not_supported_by_origin(
    SmartLeads: TypedSmartLeads,
):
    assert LeadsAisle.read is not None

    with mock.patch.object(LeadsAisle.read.criterias, "create", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.mode_not_supported_by_origin


def test_outcome_if_mode_not_supported_by_target(
    SmartLeads: TypedSmartLeads,
):
    assert JobsAisle.write is not None

    with mock.patch.object(JobsAisle.write.criterias, "create", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.mode_not_supported_by_target


def test_outcome_if_origin_auth_parameters_not_valid(
    SmartLeads: TypedSmartLeads,
):
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id="test",
        # Error below
        connector_auth=dict(smart_tag__xxxx="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        push_parameters=dict(board_key="new_board"),
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.bad_origin_parameters


def test_outcome_if_origin_non_auth_parameters_not_valid(
    SmartLeads: TypedSmartLeads,
):
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id="test",
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        # Error below
        pull_parameters=dict(city=["I should'nt be a list"]),
        push_parameters=dict(board_key="new_board"),
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.bad_origin_parameters


def test_outcome_if_target_auth_parameters_not_valid(
    SmartLeads: TypedSmartLeads,
):
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id="test",
        connector_auth=dict(smart_tag="smart::tag::smart"),
        # Error below
        hrflow_auth=dict(api_key__xxxx="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        push_parameters=dict(board_key="new_board"),
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.bad_target_parameters


def test_outcome_if_target_non_auth_parameters_not_valid(
    SmartLeads: TypedSmartLeads,
):
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id="test",
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        # Error below: board_key is mandatory
        push_parameters=dict(),
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.bad_target_parameters


def test_outcome_if_running_incremental_but_origin_does_not_support_it(
    SmartLeads: TypedSmartLeads,
):
    assert LeadsAisle.read is not None

    with mock.patch.object(LeadsAisle.read, "incremental_token_handler", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.origin_does_not_support_incremental


def test_outcome_if_running_incremental_but_no_backend_store(
    SmartLeads: TypedSmartLeads,
):
    with mock.patch("hrflow_connectors.core.backend.store", None):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.backend_not_configured


def test_outcome_if_reading_fails_completely(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    assert LeadsAisle.read is not None

    with mock.patch.object(LeadsAisle.read, "function", side_effect=Exception):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.read_failure

    assert result.events[Event.read_failure] > 0

    # incremental_token should not be persisted
    assert backend.store is not None
    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None


def test_outcome_if_getting_incremental_token_fails(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    assert LeadsAisle.read is not None

    with mock.patch.object(
        LeadsAisle.read.incremental_token_handler, "create", side_effect=Exception
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.getting_incremental_token_failure

    assert result.events[Event.read_success] > 0
    assert result.events[Event.getting_incremental_token_failure] == 1

    # incremental_token should not be persisted
    assert backend.store is not None
    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None


def test_outcome_if_logics_fails_completely(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    def failing(item: dict):
        raise Exception()

    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        push_parameters=dict(board_key="new_board"),
        logics=[failing],
        incremental=True,
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.logics_failure

    assert result.events[Event.read_success] > 0
    assert result.events[Event.logics_failure] == result.events[Event.read_success]

    # incremental_token should not be persisted
    assert backend.store is not None
    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None


def test_outcome_if_format_fails_completely(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    def failing(item: dict):
        if item.get("__hopefully_not_set") is None:
            raise Exception()
        return item

    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(city="Casablanca"),
        push_parameters=dict(board_key="new_board"),
        format=failing,
        incremental=True,
    )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.format_failure

    assert result.events[Event.read_success] > 0
    assert result.events[Event.format_failure] == result.events[Event.read_success]

    # incremental_token should not be persisted
    assert backend.store is not None
    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None


def test_outcome_if_writing_fails_completely(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    assert JobsAisle.write is not None

    with mock.patch.object(JobsAisle.write, "function", side_effect=Exception):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.write_failure

    assert result.events[Event.read_success] > 0
    assert result.events[Event.write_failure] == result.events[Event.read_success]

    # incremental_token should not be persisted
    assert backend.store is not None
    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None


def test_outcome_if_writing_fails_without_raising(
    SmartLeads: TypedSmartLeads,
):
    workflow_id = random_workflow_id()

    assert LeadsAisle.write is not None
    assert JobsAisle.write is not None

    # write is supposed to return the items for which it
    # couldn't perform the write
    # Here we return a dummy array with one but with
    # exact same size a number of read items which means
    # that it fails for 100% of items
    with (
        mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB),
        mock.patch.object(
            JobsAisle.write, "function", return_value=[1] * len(LEADS_DB)
        ),
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.fatal
    assert result.reason is Reason.write_failure
    assert result.incremental_token == str(LEADS_DB[-1]["id"])

    assert result.events[Event.read_success] > 0
    assert (
        result.events[Event.write_failure]
        == result.events[Event.read_success]
        == len(LEADS_DB)
    )

    # incremental_token **should be** persisted in this case
    assert backend.store is not None
    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(LEADS_DB[-1]["id"])


# <<< END OF FATAL SCENARIOS >>>

# <<< BELOW WE TEST VARIOUS NON FATAL / SUCCESS SCENARIOS >>>


def test_outcome_if_origin_is_empy(
    SmartLeads: TypedSmartLeads,
):
    assert JobsAisle.write is not None

    with mock.patch.object(LeadsAisle.read, "function", return_value=[]):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == 0


def test_outcome_origin_when_read_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    FAIL_AT = len(LEADS_DB) - 2

    index = 0

    def read_with_failures(*args, **kwargs):
        nonlocal index
        while index < len(LEADS_DB):
            if index == FAIL_AT:
                raise Exception()

            index += 1
            yield LEADS_DB[index - 1]

    with mock.patch.object(LeadsAisle.read, "function", new=read_with_failures):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB[:FAIL_AT])
    assert result.events[Event.read_failure] == 1
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 0


def test_outcome_origin_when_logics_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    LOGIC_1_FAIL_AT = 1

    index_1 = 0

    def logics_1_with_failure(item: dict):
        nonlocal index_1
        index_1 += 1
        if index_1 - 1 == LOGIC_1_FAIL_AT:
            raise Exception()

        return item

    LOGIC_2_FAIL_AT = len(LEADS_DB) - 2

    index_2 = 0

    def logics_2_with_failure(item: dict):
        nonlocal index_2
        index_2 += 1
        if index_2 - 1 == LOGIC_2_FAIL_AT:
            raise Exception()

        return item

    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            logics=[logics_1_with_failure, logics_2_with_failure],
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.read_failure] == 0
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 2
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 0


def test_outcome_origin_when_format_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    FAIL_FOR = [1, len(LEADS_DB) - 2]

    index = 0

    def format_with_failures(item: dict):
        nonlocal index
        index += 1
        if index - 1 in FAIL_FOR:
            raise Exception()

        return smartleads_lead_to_hrflow_job(item)

    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            format=format_with_failures,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.read_failure] == 0
    assert result.events[Event.format_failure] == len(FAIL_FOR)
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 0


def test_outcome_origin_when_write_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    # Only the len counts for the events
    failures = [1, 1]
    with (
        mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB),
        mock.patch.object(JobsAisle.write, "function", return_value=failures),
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.read_failure] == 0
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.write_failure] == len(failures)
    assert result.events[Event.callback_failure] == 0


def test_outcome_origin_when_callback_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    def failing(*args, **kwargs):
        raise Exception()

    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            callback=failing,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.read_failure] == 0
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 1


def test_outcome_origin_when_many_failure_happens(
    SmartLeads: TypedSmartLeads,
):
    FAIL_AT = len(LEADS_DB) - 1

    read_index = 0

    def read_with_failures(*args, **kwargs):
        nonlocal read_index
        while read_index < len(LEADS_DB):
            if read_index == FAIL_AT:
                raise Exception()

            read_index += 1
            yield LEADS_DB[read_index - 1]

    LOGICS_FAIL_AT = 3

    logics_index = 0

    def logics_with_failure(item: dict):
        nonlocal logics_index
        logics_index += 1
        if logics_index - 1 == LOGICS_FAIL_AT:
            raise Exception()

        return item

    FORMAT_FAIL_FOR = [1, FAIL_AT - 3]

    format_index = 0

    def format_with_failures(item: dict):
        nonlocal format_index
        format_index += 1
        if format_index - 1 in FORMAT_FAIL_FOR:
            raise Exception()

        return smartleads_lead_to_hrflow_job(item)

    def failing_callback(*args, **kwargs):
        raise Exception()

    write_failures = [1]

    with (
        mock.patch.object(LeadsAisle.read, "function", new=read_with_failures),
        mock.patch.object(JobsAisle.write, "function", return_value=write_failures),
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            format=format_with_failures,
            logics=[logics_with_failure],
            callback=failing_callback,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB[:FAIL_AT])
    assert result.events[Event.read_failure] == 1
    assert result.events[Event.format_failure] == len(FORMAT_FAIL_FOR)
    assert result.events[Event.logics_failure] == 1
    assert result.events[Event.write_failure] == len(write_failures)
    assert result.events[Event.callback_failure] == 1


def test_logics_effectively_discards_items(
    SmartLeads: TypedSmartLeads,
):
    DISCARD_FOR = [1, 3, 4]

    index = 0

    def logics(item: dict):
        nonlocal index
        index += 1
        if index - 1 in DISCARD_FOR:
            return None

        return item

    written_items = []

    def capture_written_items(*args, items, **kwargs):
        nonlocal written_items
        written_items = items
        return []

    with (
        mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB),
        mock.patch.object(JobsAisle.write, "function", new=capture_written_items),
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="test",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            logics=[logics],
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.logics_discard] == len(DISCARD_FOR)
    assert result.events[Event.write_failure] == 0

    assert written_items == [
        smartleads_lead_to_hrflow_job(item)
        for i, item in enumerate(LEADS_DB)
        if i not in DISCARD_FOR
    ]


def test_works_as_expected_with_empty_logics(
    SmartLeads: TypedSmartLeads,
):
    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            logics=[],
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0


def test_works_as_expected_with_persist_is_false(
    SmartLeads: TypedSmartLeads,
):
    with (
        mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB),
        mock.patch.object(
            JobsAisle.write, "function", side_effect=Exception
        ) as mocked_write,
    ):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id="",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
            persist=False,
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.write_failure] == 0

    mocked_write.assert_not_called()


def test_action_works_even_with_no_default_format(SmartLeadsF: SmartLeadsProto):
    SmartLeads = SmartLeadsF(
        flows=(
            Flow(
                Mode.create,
                Entity.job,
                Direction.inbound,
            ),
        )
    )

    with (
        mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB),
        mock.patch.object(JobsAisle.write, "function", return_value=[]) as mocked_write,
    ):
        result = getattr(SmartLeads, "create_jobs_in_hrflow")(
            workflow_id="",
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(city="Casablanca"),
            push_parameters=dict(board_key="new_board"),
        )

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.events[Event.write_failure] == 0

    mocked_write.assert_called_once()


# <<< END OF NON FATAL SCENARIOS >>>


def test_incremental_works_as_expected(
    SmartLeads: TypedSmartLeads,
):
    assert backend.store is not None

    workflow_id = random_workflow_id()

    last_id = 9
    LEADS_DB.append(
        dict(
            id=last_id,
            category="Corporate",
            designation="Data Manager",
            status="created",
            city="El Jadida",
            remote_allowed=True,
        ),
    )

    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None

    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(last_id)
    assert persisted.incremental is True

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.incremental_token == str(last_id)
    assert result.incremental is True

    # Rerun without changing the db should yield no reads
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == 0
    assert result.incremental is True
    assert result.incremental_token == str(last_id)

    # Add new leads with id greated than
    # last_id should yield read results
    LEADS_DB.extend(
        [
            dict(
                id=last_id + 1,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
            dict(
                id=last_id + 2,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
            dict(
                id=last_id + 3,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
        ]
    )
    new_last_id = last_id + 3

    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(new_last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == new_last_id - last_id
    assert result.incremental is True
    assert result.incremental_token == str(new_last_id)

    # Once again expecting no read
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(new_last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == 0
    assert result.incremental is True
    assert result.incremental_token == str(new_last_id)


def test_incremental_token_not_persisted_after_fatal_failure(
    SmartLeads: TypedSmartLeads,
):
    assert backend.store is not None

    workflow_id = random_workflow_id()

    last_id = 9
    LEADS_DB.append(
        dict(
            id=last_id,
            category="Corporate",
            designation="Data Manager",
            status="created",
            city="El Jadida",
            remote_allowed=True,
        ),
    )

    assert backend.store.load(key=workflow_id, parse_as=RunResult) is None

    with mock.patch.object(LeadsAisle.read, "function", return_value=LEADS_DB):
        result = SmartLeads.create_jobs_in_hrflow(
            workflow_id=workflow_id,
            connector_auth=dict(smart_tag="smart::tag::smart"),
            hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
            pull_parameters=dict(),
            push_parameters=dict(board_key="new_board"),
            incremental=True,
        )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == len(LEADS_DB)
    assert result.incremental is True
    assert result.incremental_token == str(last_id)

    # Rerun without changing the db should yield no reads
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == 0
    assert result.incremental is True
    assert result.incremental_token == str(last_id)

    # Add new leads with id greated than
    # last_id should yield read results
    LEADS_DB.extend(
        [
            dict(
                id=last_id + 1,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
            dict(
                id=last_id + 2,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
            dict(
                id=last_id + 3,
                category="Corporate",
                designation="Data Manager",
                status="created",
                city="El Jadida",
                remote_allowed=True,
            ),
        ]
    )
    new_last_id = last_id + 3

    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(new_last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == new_last_id - last_id
    assert result.incremental is True
    assert result.incremental_token == str(new_last_id)

    # Once again expecting no read
    result = SmartLeads.create_jobs_in_hrflow(
        workflow_id=workflow_id,
        connector_auth=dict(smart_tag="smart::tag::smart"),
        hrflow_auth=dict(api_key="hrflow::hrflower::hrflow"),
        pull_parameters=dict(),
        push_parameters=dict(board_key="new_board"),
        incremental=True,
    )

    persisted = backend.store.load(key=workflow_id, parse_as=RunResult)
    assert persisted is not None
    assert persisted.incremental_token == str(new_last_id)

    assert isinstance(result, RunResult)
    assert result.status is Status.success
    assert result.reason is Reason.none

    assert result.events[Event.read_success] == 0
    assert result.incremental is True
    assert result.incremental_token == str(new_last_id)
