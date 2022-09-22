import json
from collections import Counter
from pathlib import Path
from unittest import mock

import pytest
from pydantic import ValidationError

from hrflow_connectors import hrflow_connectors_manifest
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ReadMode,
    WorkflowType,
    backend,
)
from hrflow_connectors.core.connector import Event, Reason, RunResult, Status
from tests.conftest import random_workflow_id
from tests.core.localusers.warehouse import (
    FAIL_AT,
    USERS_DB,
    BadUsersWarehouse,
    FailingUsersWarehouse,
    UsersIncrementalWarehouse,
    UsersWarehouse,
    add_user,
)
from tests.core.smartleads.warehouse import (
    LEADS_DB,
    BadLeadsWarehouse,
    FailingLeadsWarehouse,
    LeadsWarehouse,
)

DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeads = Connector(
    name="SmartLeads",
    description=DESCRIPTION,
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name="pull_leads",
            trigger_type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name="same_pull_leads",
            trigger_type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name="pull_leads_incremental",
            trigger_type=WorkflowType.pull,
            description="Send users as leads with support for incremental",
            parameters=BaseActionParameters,
            origin=UsersIncrementalWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)


@pytest.fixture(autouse=True)
def reset_leads():
    LEADS_DB.clear()


@pytest.fixture
def manifest_directory():
    path = Path(__file__).parent
    yield path
    manifest = path / "manifest.json"
    manifest.unlink(missing_ok=True)


def test_connector_manifest():
    SmartLeads.manifest()


def test_hrflow_connectors_manifest(manifest_directory):
    manifest = Path(__file__).parent / "manifest.json"
    assert manifest.exists() is False

    connectors = [SmartLeads, SmartLeads]
    hrflow_connectors_manifest(connectors=connectors, directory_path=manifest_directory)

    assert manifest.exists() is True
    assert len(json.loads(manifest.read_text())["connectors"]) == len(connectors)


def test_action_by_name():
    assert SmartLeads.model.action_by_name("pull_leads") is SmartLeads.model.actions[0]
    assert (
        SmartLeads.model.action_by_name("same_pull_leads")
        is SmartLeads.model.actions[1]
    )
    assert SmartLeads.model.action_by_name("doest_not_exist") is None


def test_connector_failures():
    campaign_id = "camp_xxx1"
    result = SmartLeads.pull_leads(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=1),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_action_parameters

    result = SmartLeads.pull_leads(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(gender="M"),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_origin_parameters

    result = SmartLeads.pull_leads(
        workflow_id=random_workflow_id(),
        action_parameters=dict(),
        origin_parameters=dict(),
        target_parameters=dict(),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.bad_target_parameters

    result = SmartLeads.pull_leads(
        workflow_id=random_workflow_id(),
        action_parameters=dict(format=lambda user: 10 / 0),
        origin_parameters=dict(),
        target_parameters=dict(campaign_id=campaign_id),
    )
    assert result.status == Status.fatal
    assert result.reason == Reason.format_failure

    result = SmartLeads.pull_leads(
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=BadUsersWarehouse,
                target=LeadsWarehouse,
            ),
        ],
    )
    result = connector.pull_leads(
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
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_leads",
                    trigger_type=WorkflowType.pull,
                    description="Send users as leads",
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=BadLeadsWarehouse,
            ),
        ],
    )
    result = connector.pull_leads(
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
            description=DESCRIPTION,
            url="https://www.smartleads.test/",
            actions=[
                ConnectorAction(
                    name="pull_leads",
                    trigger_type=WorkflowType.pull,
                    description="Send users as leads",
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
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
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
    campaign_id = "camp_xxx1"

    def format(user):
        user["name"] = user["name"] + "_formatted"
        return user

    assert len(LEADS_DB[campaign_id]) == 0
    result = SmartLeads.pull_leads(
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
    campaign_id = "camp_xxx1"
    # With logic that always returns None
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
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

    result = SmartLeads.pull_leads(
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

    result = SmartLeads.pull_leads(
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
    result = SmartLeads.pull_leads(
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
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
    result = EvenSmarterLeads.pull_leads(
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
    result = EvenSmarterLeads.pull_leads(
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=FailingUsersWarehouse,
                target=FailingLeadsWarehouse,
            ),
        ],
    )
    # Without default format
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = FailingSmartLeads.pull_leads(
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads_with_callback",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=LeadsWarehouse,
                callback=callback,
            ),
        ],
    )

    result = SmartLeads.pull_leads_with_callback(
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
        description=DESCRIPTION,
        url="https://www.smartleads.test/",
        actions=[
            ConnectorAction(
                name="pull_leads_with_callback",
                trigger_type=WorkflowType.pull,
                description="Send users as leads",
                parameters=BaseActionParameters,
                origin=UsersWarehouse,
                target=LeadsWarehouse,
                callback=callback,
            ),
        ],
    )

    campaign_id = "camp_xxx1"

    result = SmartLeads.pull_leads_with_callback(
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
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    result = SmartLeads.pull_leads(
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
    campaign_id = "camp_xxx1"
    assert len(LEADS_DB[campaign_id]) == 0

    with mock.patch.object(backend, "is_configured", new=False):
        with mock.patch.object(
            SmartLeads.model.actions[0].origin.read, "supports_incremental", new=True
        ):
            result = SmartLeads.pull_leads(
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
    campaign_id = "camp_xxx21"
    assert len(LEADS_DB[campaign_id]) == 0

    workflow_id = random_workflow_id()
    with mock.patch.object(
        SmartLeads.model.actions[2].origin.read,
        "item_to_read_from",
        new=lambda item: 1 / 0,
    ):
        result = SmartLeads.pull_leads_incremental(
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
    campaign_id = "camp_xxx35"
    assert len(LEADS_DB[campaign_id]) == 0

    initial_users = len(USERS_DB)

    workflow_id = random_workflow_id()
    result = SmartLeads.pull_leads_incremental(
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

    result = SmartLeads.pull_leads_incremental(
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
    campaign_id = "camp_xxx35"
    assert len(LEADS_DB[campaign_id]) == 0

    workflow_id = random_workflow_id()
    result = SmartLeads.pull_leads_incremental(
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
    result = SmartLeads.pull_leads_incremental(
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
    result = SmartLeads.pull_leads_incremental(
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

    result = SmartLeads.pull_leads_incremental(
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

    result = SmartLeads.pull_leads_incremental(
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
        result = SmartLeads.pull_leads_incremental(
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
        result = SmartLeads.pull_leads_incremental(
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
        result = SmartLeads.pull_leads_incremental(
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
