from collections import Counter
from unittest import mock

import pytest
from pydantic import ValidationError

from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    ParametersOverride,
    ReadMode,
    WorkflowType,
    backend,
)
from hrflow_connectors.core.connector import Event, Reason, RunResult, Status
from tests.conftest import random_workflow_id
from tests.core.src.hrflow_connectors.connectors.localusers.warehouse import (
    FAIL_AT,
    USERS_DB,
    BadUsersWarehouse,
    FailingUsersWarehouse,
    UsersIncrementalWarehouse,
    UsersWarehouse,
    add_user,
)
from tests.core.src.hrflow_connectors.connectors.smartleads.warehouse import (
    LEADS_DB,
    BadLeadsWarehouse,
    FailingLeadsWarehouse,
    LeadsWarehouse,
)

DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeadsF = lambda: Connector(
    name="SmartLeads",
    type=ConnectorType.Other,
    subtype="smartleads",
    description=DESCRIPTION,
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name=ActionName.push_profile,
            action_type=ActionType.inbound,
            trigger_type=WorkflowType.pull,
            description="Test action",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name=ActionName.push_job_list,
            action_type=ActionType.inbound,
            trigger_type=WorkflowType.pull,
            description="Same as first action",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name=ActionName.push_profile_list,
            action_type=ActionType.inbound,
            trigger_type=WorkflowType.pull,
            description="Action with support for incremental",
            parameters=BaseActionParameters,
            origin=UsersIncrementalWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)


@pytest.fixture(autouse=True)
def reset_leads():
    LEADS_DB.clear()


def test_action_by_name():
    SmartLeads = SmartLeadsF()
    assert (
        SmartLeads.model.action_by_name("push_profile") is SmartLeads.model.actions[0]
    )
    assert (
        SmartLeads.model.action_by_name("push_job_list") is SmartLeads.model.actions[1]
    )
    assert SmartLeads.model.action_by_name("doest_not_exist") is None


def test_action_name_constraint():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="not_a_valid_action_name",
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.pull,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("name",)
    assert errors[0]["msg"].startswith("value is not a valid enumeration member;")


def test_action_pull_profile_list_only_with_trigger_type_pull():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_profile_list",
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.catch,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("name",)
    assert errors[0][
        "msg"
    ] == "`pull_application_list`, `pull_job_list` and `pull_profile_list` are only available for trigger_type={}".format(  # noqa: E501
        WorkflowType.pull
    )


def test_action_pull_job_list_only_with_trigger_type_pull():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_job_list",
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.catch,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("name",)
    assert errors[0][
        "msg"
    ] == "`pull_application_list`, `pull_job_list` and `pull_profile_list` are only available for trigger_type={}".format(  # noqa: E501
        WorkflowType.pull
    )


def test_action_pull_application_list_only_with_trigger_type_pull():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_application_list",
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.catch,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("name",)
    assert errors[0][
        "msg"
    ] == "`pull_application_list`, `pull_job_list` and `pull_profile_list` are only available for trigger_type={}".format(  # noqa: E501
        WorkflowType.pull
    )


def test_connector_failures():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"
    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=1),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_action_parameters

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(gender="M"),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_origin_parameters

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_target_parameters

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=lambda user: 10 / 0),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.format_failure

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(logics=[lambda user: 10 / 0]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.logics_failure


def test_origin_warehouse_failure():
    connector = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters,
                origin=BadUsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    result = connector.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id="camp_xxx1"),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.read_failure
    assert result.events[Event.read_success] == 0
    assert result.events[Event.read_failure] == 1


def test_origin_not_readable_failure():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name=ActionName.push_profile,
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.pull,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=LeadsWarehouse,
                    target=LeadsWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("origin",)
    assert errors[0]["msg"] == "Origin warehouse is not readable"


def test_target_warehouse_failure():
    connector = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=BadLeadsWarehouse,
            ),
        ],
    )
    result = connector.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id="camp_xxx1"),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.write_failure
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.write_failure] == result.events[Event.read_success]


def test_target_not_writable_failure():
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name=ActionName.push_profile,
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.pull,
                    description="Test action",
                    parameters=BaseActionParameters,
                    origin=UsersWarehouse,
                    target=UsersWarehouse,
                ),
            ],
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("target",)
    assert errors[0]["msg"] == "Target warehouse is not writable"


def test_connector_simple_success():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])


def test_connector_with_format():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"

    def format(user):
        user["name"] = user["name"] + "_formatted"
        return user

    assert len(LEADS_DB[campaign_id]) == 0
    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=format),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])

    assert [lead["name"].endswith("_formatted") for lead in LEADS_DB[campaign_id]]


def test_connector_with_logics():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"
    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(logics=[lambda u: u, lambda u: None, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == len(USERS_DB)
    assert result.events[Event.write_failure] == 0

    assert len(LEADS_DB[campaign_id]) == 0

    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: None]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == len(USERS_DB)
    assert result.events[Event.write_failure] == 0

    assert len(LEADS_DB[campaign_id]) == 0

    # With pass-through logics
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(logics=[lambda u: u, lambda u: u, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])

    # With only males logic
    campaign_id = "camp_xxx2"
    n_males = len([u for u in USERS_DB if u["gender"] == "male"])

    def logic(user):
        if user["gender"] == "male":
            return user

    assert len(LEADS_DB[campaign_id]) == 0
    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(logics=[lambda u: u, logic, lambda u: u]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == len(USERS_DB) - n_males
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) != len(LEADS_DB[campaign_id])
    assert n_males == len(LEADS_DB[campaign_id])


def test_connector_default_format():
    def smarter_format(user):
        user["name"] = user["name"].upper()
        return user

    EvenSmarterLeads = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters.with_defaults(
                    "even_smarter_leads_connector", format=smarter_format
                ),
                origin=UsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    # Without default format
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0
    result = EvenSmarterLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=lambda user: user),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert any(lead["name"].upper() != lead["name"] for lead in LEADS_DB[campaign_id])

    # With default format
    campaign_id = "camp_xxx2"
    assert len(LEADS_DB[campaign_id]) == 0
    result = EvenSmarterLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])
    assert all(lead["name"].upper() == lead["name"] for lead in LEADS_DB[campaign_id])


def test_connector_default_event_parser():
    def event_parser(event):
        return

    without_event_parser = BaseActionParameters.with_defaults("no_change")
    assert without_event_parser.__fields__["event_parser"].default is None

    with_event_parser = BaseActionParameters.with_defaults(
        "with_event_parser", event_parser=event_parser
    )
    assert with_event_parser.__fields__["event_parser"].default is event_parser


def test_action_with_failures():
    def failing_format(user):
        if user["name"] in [USERS_DB[0]["name"], USERS_DB[3]["name"]]:
            (10 / 0)
        user["name"] = user["name"].upper()
        return user

    def failing_logic(user):
        if user["name"] in [USERS_DB[2]["name"], USERS_DB[5]["name"]]:
            (10 / 0)
        return user

    FailingSmartLeads = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters,
                origin=FailingUsersWarehouse,
                target=FailingLeadsWarehouse,
            ),
        ],
    )
    # Without default format
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = FailingSmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=failing_format, logics=[failing_logic]),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success_with_failures
    assert result.events[Event.read_success] == FAIL_AT
    assert result.events[Event.read_failure] == 1
    assert result.events[Event.format_failure] == 2
    assert result.events[Event.logics_failure] == 2
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 2

    assert len(LEADS_DB[campaign_id]) == FAIL_AT - 2 - 2 - 2
    assert all(lead["name"].upper() == lead["name"] for lead in LEADS_DB[campaign_id])


def test_action_run_results_from_events():
    result = RunResult.from_events(
        Counter({Event.read_success: 0, Event.read_failure: 0})
    )
    assert result.status is Status.success
    assert result.reason is Reason.none

    result = RunResult.from_events(
        Counter({Event.read_success: 0, Event.read_failure: 1})
    )
    assert result.status is Status.fatal
    assert result.reason is Reason.read_failure

    result = RunResult.from_events(
        Counter({Event.read_success: 5, Event.read_failure: 1})
    )
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    result = RunResult.from_events(
        Counter(
            {
                Event.read_success: 5,
                Event.read_failure: 1,
                Event.format_failure: 5,
            }
        )
    )
    assert result.status is Status.fatal
    assert result.reason is Reason.format_failure

    result = RunResult.from_events(
        Counter(
            {
                Event.read_success: 5,
                Event.read_failure: 1,
                Event.format_failure: 1,
                Event.logics_failure: 4,
            }
        )
    )
    assert result.status is Status.fatal
    assert result.reason is Reason.logics_failure

    result = RunResult.from_events(
        Counter(
            {
                Event.read_success: 5,
                Event.read_failure: 1,
                Event.format_failure: 1,
                Event.logics_discard: 1,
                Event.logics_failure: 3,
            }
        )
    )
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none

    result = RunResult.from_events(
        Counter(
            {
                Event.read_success: 5,
                Event.read_failure: 1,
                Event.format_failure: 1,
                Event.logics_discard: 1,
                Event.logics_failure: 1,
                Event.write_failure: 2,
            }
        )
    )
    assert result.status is Status.fatal
    assert result.reason is Reason.write_failure

    result = RunResult.from_events(
        Counter(
            {
                Event.read_success: 5,
                Event.read_failure: 1,
                Event.format_failure: 1,
                Event.logics_discard: 1,
                Event.logics_failure: 1,
                Event.write_failure: 1,
            }
        )
    )
    assert result.status is Status.success_with_failures
    assert result.reason is Reason.none


def test_action_with_callback_success():
    change_me = False
    campaign_id = "camp_xxx1"
    n_males = len([user for user in USERS_DB if user["gender"].value == "male"])

    def callback(origin_parameters, target_parameters, events, written_items) -> None:
        nonlocal change_me
        change_me = True
        assert events[Event.read_success] == n_males
        assert events[Event.format_failure] == 0
        assert events[Event.logics_failure] == 0
        assert events[Event.logics_discard] == 0
        assert events[Event.write_failure] == 0
        assert len(written_items) == n_males
        assert origin_parameters.gender.value == "male"
        assert target_parameters.campaign_id == campaign_id

    SmartLeads = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=LeadsWarehouse,
                callback=callback,
            ),
        ],
    )

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(gender="male"),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert change_me is True

    assert result.status == Status.success
    assert result.events[Event.read_success] == n_males
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 0
    assert result.events[Event.callback_executed] == 1

    assert n_males == len(LEADS_DB[campaign_id])


def test_action_with_callback_failure():
    def callback(origin_parameters, target_parameters, events, written_items) -> None:
        raise Exception("Callback failure")

    SmartLeads = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name=ActionName.push_profile,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Test action",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=LeadsWarehouse,
                callback=callback,
            ),
        ],
    )

    campaign_id = "camp_xxx1"

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success_with_failures
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0
    assert result.events[Event.callback_failure] == 1
    assert result.events[Event.callback_executed] == 1

    assert len(USERS_DB) == len(LEADS_DB[campaign_id])


def test_connector_incremental_read_not_supported():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.push_profile(
        workflow_id=random_workflow_id(),
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.fatal
    assert result.reason == Reason.origin_does_not_support_incremental
    assert result.events[Event.read_success] == 0
    assert len(LEADS_DB[campaign_id]) == 0


def test_connector_incremental_backend_not_configured():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    with mock.patch.object(backend, "is_configured", new=False):
        with mock.patch.object(
            SmartLeads.model.actions[0].origin.read, "supports_incremental", new=True
        ):
            result = SmartLeads.push_profile(
                workflow_id=random_workflow_id(),
                action_parameters=dict(read_mode=ReadMode.incremental),
                origin_parameters=dict(),
                target_parameters=dict(campaign_id=campaign_id),
            )

    assert result.status == Status.fatal
    assert result.reason == Reason.backend_not_configured_in_incremental_mode
    assert result.events[Event.read_success] == 0
    assert len(LEADS_DB[campaign_id]) == 0


def test_connector_incremental_item_to_read_from_failing():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx21"
    assert len(LEADS_DB[campaign_id]) == 0

    workflow_id = random_workflow_id()
    with mock.patch.object(
        SmartLeads.model.actions[2].origin.read,
        "item_to_read_from",
        new=lambda item: 1 / 0,
    ):
        result = SmartLeads.push_profile_list(
            workflow_id=workflow_id,
            action_parameters=dict(read_mode=ReadMode.incremental),
            origin_parameters=dict(),
            target_parameters=dict(campaign_id=campaign_id),
        )

    assert result.status == Status.fatal
    assert result.reason == Reason.item_to_read_from_failure
    assert result.events[Event.read_success] == len(USERS_DB)
    assert result.read_from is None


def test_connector_incremental_read():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx35"
    assert len(LEADS_DB[campaign_id]) == 0

    initial_users = len(USERS_DB)

    workflow_id = random_workflow_id()
    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == initial_users
    assert len(LEADS_DB[campaign_id]) == initial_users
    assert result.read_from == str(USERS_DB[-1]["id"])

    # Add user to USERS_DB
    add_user()
    add_user()
    last_id = add_user()["id"]

    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == 3
    assert len(LEADS_DB[campaign_id]) == initial_users + 3
    assert result.read_from == str(last_id)


def test_read_from_is_persisted_after_failure():
    SmartLeads = SmartLeadsF()
    campaign_id = "camp_xxx35"
    assert len(LEADS_DB[campaign_id]) == 0

    workflow_id = random_workflow_id()
    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )

    assert result.status == Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)
    assert result.read_from == str(USERS_DB[-1]["id"])

    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    # Init error
    init_error = mock.MagicMock()
    init_error.reason = Reason.workflow_id_not_found
    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
        init_error=init_error,
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.workflow_id_not_found
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    # Bad paramaters
    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=5),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_action_parameters
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(gender="XFRE"),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_origin_parameters
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    result = SmartLeads.push_profile_list(
        workflow_id=workflow_id,
        action_parameters=dict(read_mode=ReadMode.incremental),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=[1, 2, 3]),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_target_parameters
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    # Backend not configured
    with mock.patch.object(backend, "is_configured", new=False):
        result = SmartLeads.push_profile_list(
            workflow_id=workflow_id,
            action_parameters=dict(read_mode=ReadMode.incremental),
            origin_parameters=dict(),
            target_parameters=dict(campaign_id=campaign_id),
        )
    assert result.status == Status.fatal
    assert result.reason == Reason.backend_not_configured_in_incremental_mode
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    # Read failure
    with mock.patch.object(
        SmartLeads.model.actions[2].origin.read,
        "function",
        new=lambda *args, **kwargs: 1 / 0,
    ):
        result = SmartLeads.push_profile_list(
            workflow_id=workflow_id,
            action_parameters=dict(read_mode=ReadMode.incremental),
            origin_parameters=dict(),
            target_parameters=dict(campaign_id=campaign_id),
        )
    assert result.status == Status.fatal
    assert result.reason == Reason.read_failure
    assert backend.store.load(key=workflow_id, parse_as=RunResult).read_from == str(
        USERS_DB[-1]["id"]
    )

    # Item to read_from failure
    last_id = add_user()["id"]
    with mock.patch.object(
        SmartLeads.model.actions[2].origin.read,
        "item_to_read_from",
        new=lambda item: 1 / 0,
    ):
        result = SmartLeads.push_profile_list(
            workflow_id=workflow_id,
            action_parameters=dict(read_mode=ReadMode.incremental),
            origin_parameters=dict(),
            target_parameters=dict(campaign_id=campaign_id),
        )

    assert result.status == Status.fatal
    assert result.events[Event.read_success] == 1
    assert result.reason == Reason.item_to_read_from_failure
    assert (
        backend.store.load(key=workflow_id, parse_as=RunResult).read_from
        == str(USERS_DB[-2]["id"])
        != last_id
    )


def test_no_empty_parameters_overrride():
    with pytest.raises(ValidationError) as excinfo:
        ParametersOverride(
            name=ActionName.push_job_list, format=None, event_parser=None
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("__root__",)
    assert errors[0]["msg"].startswith(
        "One of `format` or `event_parser` should not be None"
    )

    with pytest.raises(ValidationError) as excinfo:
        ParametersOverride(
            name=ActionName.push_job_list,
        )

    errors = excinfo.value.errors()
    assert errors[0]["loc"] == ("__root__",)
    assert errors[0]["msg"].startswith(
        "One of `format` or `event_parser` should not be None"
    )


def test_connector_action_based_on_no_override_simple():
    original = ConnectorAction(
        name=ActionName.push_profile,
        action_type=ActionType.inbound,
        trigger_type=WorkflowType.pull,
        description="Test action",
        parameters=BaseActionParameters,
        origin=UsersWarehouse,
        target=LeadsWarehouse,
    )
    based = ConnectorAction.based_on(
        base=original,
        connector_name="ZYX Inc",
    )
    assert based.name is original.name
    assert based.action_type is original.action_type
    assert based.trigger_type is original.trigger_type
    assert based.origin == original.origin
    assert based.target == original.target
    assert based.parameters.__fields__.keys() == original.parameters.__fields__.keys()
    for key, field in based.parameters.__fields__.items():
        original_field = original.parameters.__fields__[key]
        assert field.name == original_field.name
        assert field.type_ == original_field.type_
        assert field.required == original_field.required
        assert field.default == original_field.default


def test_connector_action_based_on_no_override_with_defaults():
    def default_format(*args, **kwargs):
        pass

    def default_event_parser(*args, **kwargs):
        pass

    original = ConnectorAction(
        name=ActionName.push_profile,
        action_type=ActionType.inbound,
        trigger_type=WorkflowType.pull,
        description="Test action",
        parameters=BaseActionParameters.with_defaults(
            "WithDefault", format=default_format, event_parser=default_event_parser
        ),
        origin=UsersWarehouse,
        target=LeadsWarehouse,
    )
    based = ConnectorAction.based_on(
        base=original,
        connector_name="ZYX Inc",
    )
    assert based.name is original.name
    assert based.action_type is original.action_type
    assert based.trigger_type is original.trigger_type
    assert based.origin == original.origin
    assert based.target == original.target
    assert based.parameters.__fields__.keys() == original.parameters.__fields__.keys()
    for key, field in based.parameters.__fields__.items():
        original_field = original.parameters.__fields__[key]
        assert field.name == original_field.name
        assert field.type_ == original_field.type_
        assert field.required == original_field.required
        assert field.default == original_field.default


def test_connector_action_based_on_format_override_with_defaults():
    def default_format(*args, **kwargs):
        pass

    def new_format(*args, **kwargs):
        pass

    def default_event_parser(*args, **kwargs):
        pass

    original = ConnectorAction(
        name=ActionName.push_profile,
        action_type=ActionType.inbound,
        trigger_type=WorkflowType.pull,
        description="Test action",
        parameters=BaseActionParameters.with_defaults(
            "WithDefault", format=default_format, event_parser=default_event_parser
        ),
        origin=UsersWarehouse,
        target=LeadsWarehouse,
    )
    based = ConnectorAction.based_on(
        base=original, connector_name="ZYX Inc", with_format=new_format
    )
    assert based.name is original.name
    assert based.action_type is original.action_type
    assert based.trigger_type is original.trigger_type
    assert based.origin == original.origin
    assert based.target == original.target
    assert based.parameters.__fields__.keys() == original.parameters.__fields__.keys()
    for key, field in based.parameters.__fields__.items():
        original_field = original.parameters.__fields__[key]
        assert field.name == original_field.name
        assert field.type_ == original_field.type_
        assert field.required == original_field.required
        if key != "format":
            assert field.default == original_field.default
        else:
            assert field.default is new_format
            assert original_field.default is default_format


def test_connector_action_based_on_event_parser_override_with_defaults():
    def default_format(*args, **kwargs):
        pass

    def default_event_parser(*args, **kwargs):
        pass

    def new_event_parser(*args, **kwargs):
        pass

    original = ConnectorAction(
        name=ActionName.push_profile,
        action_type=ActionType.inbound,
        trigger_type=WorkflowType.pull,
        description="Test action",
        parameters=BaseActionParameters.with_defaults(
            "WithDefault", format=default_format, event_parser=default_event_parser
        ),
        origin=UsersWarehouse,
        target=LeadsWarehouse,
    )
    based = ConnectorAction.based_on(
        base=original, connector_name="ZYX Inc", with_event_parser=new_event_parser
    )
    assert based.name is original.name
    assert based.action_type is original.action_type
    assert based.trigger_type is original.trigger_type
    assert based.origin == original.origin
    assert based.target == original.target
    assert based.parameters.__fields__.keys() == original.parameters.__fields__.keys()
    for key, field in based.parameters.__fields__.items():
        original_field = original.parameters.__fields__[key]
        assert field.name == original_field.name
        assert field.type_ == original_field.type_
        assert field.required == original_field.required
        if key != "event_parser":
            assert field.default == original_field.default
        else:
            assert field.default is new_event_parser
            assert original_field.default is default_event_parser


def test_connector_action_based_on_full_override_with_defaults():
    def default_format(*args, **kwargs):
        pass

    def default_event_parser(*args, **kwargs):
        pass

    def new_format(*args, **kwargs):
        pass

    def new_event_parser(*args, **kwargs):
        pass

    original = ConnectorAction(
        name=ActionName.push_profile,
        action_type=ActionType.inbound,
        trigger_type=WorkflowType.pull,
        description="Test action",
        parameters=BaseActionParameters.with_defaults(
            "WithDefault", format=default_format, event_parser=default_event_parser
        ),
        origin=UsersWarehouse,
        target=LeadsWarehouse,
    )
    based = ConnectorAction.based_on(
        base=original,
        connector_name="ZYX Inc",
        with_format=new_format,
        with_event_parser=new_event_parser,
    )
    assert based.name is original.name
    assert based.action_type is original.action_type
    assert based.trigger_type is original.trigger_type
    assert based.origin == original.origin
    assert based.target == original.target
    assert based.parameters.__fields__.keys() == original.parameters.__fields__.keys()
    for key, field in based.parameters.__fields__.items():
        original_field = original.parameters.__fields__[key]
        assert field.name == original_field.name
        assert field.type_ == original_field.type_
        assert field.required == original_field.required
        if key not in ["event_parser", "format"]:
            assert field.default == original_field.default
        elif key == "event_parser":
            assert field.default is new_event_parser
            assert original_field.default is default_event_parser
        elif key == "format":
            assert field.default is new_format
            assert original_field.default is default_format


def test_connector_based_on_simple():
    SmartLeads = SmartLeadsF()

    SmartLeadsCopy = Connector.based_on(
        base=SmartLeads,
        name="SmartLeadsCopy",
        type=ConnectorType.Other,
        description="SmartLeadsCopy",
        url="Some URL",
    )
    assert SmartLeadsCopy.model != SmartLeads.model

    assert SmartLeadsCopy.model.name == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.description == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.url == "Some URL"
    assert SmartLeadsCopy.model.actions == SmartLeads.model.actions
    for action in SmartLeads.model.actions:
        assert hasattr(SmartLeadsCopy, action.name.name)


def test_connector_based_on_action_override():
    SmartLeads = SmartLeadsF()

    action_to_override = ActionName.push_profile
    assert hasattr(SmartLeads, action_to_override.name)

    SmartLeadsCopy = Connector.based_on(
        base=SmartLeads,
        name="SmartLeadsCopy",
        type=ConnectorType.Other,
        description="SmartLeadsCopy",
        url="Some URL",
        with_actions=[
            ConnectorAction(
                name=action_to_override,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Action with new description",
                parameters=BaseActionParameters,
                origin=BadUsersWarehouse,
                target=LeadsWarehouse,
            )
        ],
    )
    assert SmartLeadsCopy.model != SmartLeads.model

    assert SmartLeadsCopy.model.name == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.description == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.url == "Some URL"
    assert len(SmartLeadsCopy.model.actions) == len(SmartLeads.model.actions)
    for action in SmartLeads.model.actions:
        assert hasattr(SmartLeadsCopy, action.name.value)

    new_action = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is action_to_override
        )
    )
    assert new_action.description == "Action with new description"
    assert new_action.origin == BadUsersWarehouse
    assert new_action.target == LeadsWarehouse


def test_connector_based_on_new_action():
    SmartLeads = SmartLeadsF()

    new_action_name = ActionName.pull_resume_attachment_list
    assert not hasattr(SmartLeads, new_action_name.name)

    SmartLeadsCopy = Connector.based_on(
        base=SmartLeads,
        name="SmartLeadsCopy",
        type=ConnectorType.Other,
        description="SmartLeadsCopy",
        url="Some URL",
        with_actions=[
            ConnectorAction(
                name=new_action_name,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="New action",
                parameters=BaseActionParameters,
                origin=UsersIncrementalWarehouse,
                target=FailingLeadsWarehouse,
            )
        ],
    )
    assert SmartLeadsCopy.model != SmartLeads.model

    assert SmartLeadsCopy.model.name == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.description == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.url == "Some URL"
    assert len(SmartLeadsCopy.model.actions) == len(SmartLeads.model.actions) + 1
    for action in SmartLeads.model.actions:
        assert hasattr(SmartLeadsCopy, action.name.value)

    new_action = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is new_action_name
        )
    )
    assert new_action.description == "New action"
    assert new_action.origin == UsersIncrementalWarehouse
    assert new_action.target == FailingLeadsWarehouse

    SmartLeadsCopy.model.actions.remove(new_action)
    assert SmartLeadsCopy.model.actions == SmartLeads.model.actions


def test_connector_based_on_parameters_override():
    SmartLeads = SmartLeadsF()

    parameters_action_name = ActionName.push_profile_list
    assert hasattr(SmartLeads, parameters_action_name.name)

    def new_format(*args, **kwargs):
        pass

    def new_event_parser(*args, **kwargs):
        pass

    SmartLeadsCopy = Connector.based_on(
        base=SmartLeads,
        name="SmartLeadsCopy",
        type=ConnectorType.Other,
        description="SmartLeadsCopy",
        url="Some URL",
        with_parameters_override=[
            ParametersOverride(
                name=parameters_action_name,
                format=new_format,
                event_parser=new_event_parser,
            )
        ],
    )
    assert SmartLeadsCopy.model != SmartLeads.model

    assert SmartLeadsCopy.model.name == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.description == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.url == "Some URL"
    assert len(SmartLeadsCopy.model.actions) == len(SmartLeads.model.actions)

    new_action = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is parameters_action_name
        )
    )
    SmartLeadsCopy.model.actions.remove(new_action)
    original_action = next(
        (
            action
            for action in SmartLeads.model.actions
            if action.name is parameters_action_name
        )
    )
    SmartLeads.model.actions.remove(original_action)

    assert SmartLeadsCopy.model.actions == SmartLeads.model.actions

    assert new_action.name == original_action.name
    assert new_action.trigger_type == original_action.trigger_type
    assert new_action.description == original_action.description
    assert new_action.origin == original_action.origin
    assert new_action.target == original_action.target
    assert new_action.callback == original_action.callback
    assert new_action.action_type == original_action.action_type

    assert new_action.parameters != original_action.parameters


def test_connector_based_on_parameters_override_and_new_action():
    SmartLeads = SmartLeadsF()

    parameters_action_name = ActionName.push_profile_list
    assert hasattr(SmartLeads, parameters_action_name.name)

    new_action_name = ActionName.pull_resume_attachment_list
    assert not hasattr(SmartLeads, new_action_name.name)

    action_to_override = ActionName.push_profile
    assert hasattr(SmartLeads, action_to_override.name)

    def new_format(*args, **kwargs):
        pass

    def new_event_parser(*args, **kwargs):
        pass

    SmartLeadsCopy = Connector.based_on(
        base=SmartLeads,
        name="SmartLeadsCopy",
        type=ConnectorType.Other,
        description="SmartLeadsCopy",
        url="Some URL",
        with_parameters_override=[
            ParametersOverride(
                name=parameters_action_name,
                format=new_format,
                event_parser=new_event_parser,
            )
        ],
        with_actions=[
            ConnectorAction(
                name=new_action_name,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="New action",
                parameters=BaseActionParameters,
                origin=UsersIncrementalWarehouse,
                target=FailingLeadsWarehouse,
            ),
            ConnectorAction(
                name=action_to_override,
                action_type=ActionType.inbound,
                trigger_type=WorkflowType.pull,
                description="Action with new description",
                parameters=BaseActionParameters,
                origin=BadUsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    assert SmartLeadsCopy.model != SmartLeads.model

    assert SmartLeadsCopy.model.name == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.description == "SmartLeadsCopy"
    assert SmartLeadsCopy.model.url == "Some URL"
    assert len(SmartLeadsCopy.model.actions) == len(SmartLeads.model.actions) + 1

    for action in SmartLeads.model.actions:
        assert hasattr(SmartLeadsCopy, action.name.value)

    # Parameters Part
    action_with_overriden_parameters = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is parameters_action_name
        )
    )
    SmartLeadsCopy.model.actions.remove(action_with_overriden_parameters)

    action_with_original_parameters = next(
        (
            action
            for action in SmartLeads.model.actions
            if action.name is parameters_action_name
        )
    )
    SmartLeads.model.actions.remove(action_with_original_parameters)

    assert action_with_overriden_parameters.name == action_with_original_parameters.name
    assert (
        action_with_overriden_parameters.trigger_type
        == action_with_original_parameters.trigger_type
    )
    assert (
        action_with_overriden_parameters.description
        == action_with_original_parameters.description
    )
    assert (
        action_with_overriden_parameters.origin
        == action_with_original_parameters.origin
    )
    assert (
        action_with_overriden_parameters.target
        == action_with_original_parameters.target
    )
    assert (
        action_with_overriden_parameters.callback
        == action_with_original_parameters.callback
    )
    assert (
        action_with_overriden_parameters.action_type
        == action_with_original_parameters.action_type
    )

    assert (
        action_with_overriden_parameters.parameters
        != action_with_original_parameters.parameters
    )

    # New Action Part
    new_action = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is new_action_name
        )
    )
    SmartLeadsCopy.model.actions.remove(new_action)
    assert new_action.description == "New action"
    assert new_action.origin == UsersIncrementalWarehouse
    assert new_action.target == FailingLeadsWarehouse

    # Action override Part
    overriden_action = next(
        (
            action
            for action in SmartLeadsCopy.model.actions
            if action.name is action_to_override
        )
    )
    SmartLeadsCopy.model.actions.remove(overriden_action)

    original_action = next(
        (
            action
            for action in SmartLeads.model.actions
            if action.name is action_to_override
        )
    )
    SmartLeads.model.actions.remove(original_action)

    assert overriden_action.description == "Action with new description"
    assert overriden_action.origin == BadUsersWarehouse
    assert overriden_action.target == LeadsWarehouse

    assert SmartLeadsCopy.model.actions == SmartLeads.model.actions


def test_connector_based_on_bad_parameters_override():
    SmartLeads = SmartLeadsF()

    parameters_action_name = ActionName.push_job
    assert not hasattr(SmartLeads, parameters_action_name.name)

    def new_format(*args, **kwargs):
        pass

    with pytest.raises(ValueError) as excinfo:
        Connector.based_on(
            base=SmartLeads,
            name="SmartLeadsCopy",
            type=ConnectorType.Other,
            description="SmartLeadsCopy",
            url="Some URL",
            with_parameters_override=[
                ParametersOverride(
                    name=parameters_action_name,
                    format=new_format,
                )
            ],
        )

    assert str(
        excinfo.value
    ) == "Base connector does not have a {} action to override".format(
        parameters_action_name.name
    )


def test_connector_based_on_duplicate_action():
    SmartLeads = SmartLeadsF()

    parameters_action_name = ActionName.push_profile_list
    assert hasattr(SmartLeads, parameters_action_name.name)

    def new_format(*args, **kwargs):
        pass

    with pytest.raises(ValueError) as excinfo:
        Connector.based_on(
            base=SmartLeads,
            name="SmartLeadsCopy",
            type=ConnectorType.Other,
            description="SmartLeadsCopy",
            url="Some URL",
            with_parameters_override=[
                ParametersOverride(
                    name=parameters_action_name,
                    format=new_format,
                )
            ],
            with_actions=[
                ConnectorAction(
                    name=parameters_action_name,
                    action_type=ActionType.inbound,
                    trigger_type=WorkflowType.pull,
                    description="New action",
                    parameters=BaseActionParameters,
                    origin=UsersIncrementalWarehouse,
                    target=FailingLeadsWarehouse,
                ),
            ],
        )

    assert str(excinfo.value) == (
        "Duplicate action name {} in `with_parameters_override` and `with_actions`"
    ).format(parameters_action_name.name)


def test_connector_model_subtype_missing():
    model = Connector(
        name="SmartLeads",
        type=ConnectorType.Other,
        subtype="smartleads",
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[],
    )
    assert model.model.subtype == "smartleads"


def test_connector_subtype_constraint():
    subtype = "Smart Leads"
    with pytest.raises(ValidationError) as excinfo:
        Connector(
            name="SmartLeads",
            type=ConnectorType.Other,
            subtype="smartleads",
            subtype=subtype,
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[],
        )
    expected_error = (
        "1 validation error for ConnectorModel\nsubtype\n  ConnectorModel's"
        " `subtype`={} must be lowercase without any spaces. (type=value_error)".format(
            subtype
        )
    )
    assert str(excinfo.value) == expected_error
