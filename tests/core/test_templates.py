import pytest

import hrflow_connectors
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.core.connector import Event, Reason, Status
from tests.core.localusers.warehouse import USERS_DB, Gender, UsersWarehouse
from tests.core.smartleads.warehouse import LEADS_DB, LeadsWarehouse

DESCRIPTION = "Test Connector for seamless users to leads integration"

SmartLeads = Connector(
    name="SmartLeads",
    description=DESCRIPTION,
    url="https://www.smartleads.test/",
    actions=[
        ConnectorAction(
            name="pull_leads",
            type=WorkflowType.pull,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
        ConnectorAction(
            name="catch_user",
            type=WorkflowType.catch,
            description="Send users as leads",
            parameters=BaseActionParameters,
            origin=UsersWarehouse,
            target=LeadsWarehouse,
        ),
    ],
)


@pytest.fixture
def with_smartleads():
    setattr(hrflow_connectors, "SmartLeads", SmartLeads)
    yield hrflow_connectors
    delattr(hrflow_connectors, "SmartLeads")


def test_pull_workflow_code(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][0]
    assert action_manifest["name"] == "pull_leads"

    campaign_id = "xxxx_234"
    n_males = len([u for u in USERS_DB if u["gender"] is Gender.male])

    workflow_code = action_manifest["workflow_code"]
    script = (
        workflow_code
        + "\n__run_result=workflow(settings=dict({origin_prefix}gender='male',"
        " {target_prefix}campaign_id='{campaign_id}'))".format(
            origin_prefix=action_manifest["workflow_code_origin_settings_prefix"],
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    result = global_namespace["__run_result"]
    assert result.status is Status.success
    assert result.reason is Reason.none
    assert result.events[Event.read_success] == n_males
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0
    assert len(LEADS_DB[campaign_id]) == n_males


def test_pull_workflow_code_with_format(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][0]
    assert action_manifest["name"] == "pull_leads"

    campaign_id = "xxxx_7875"
    format_function = """
def format(item):
    global change_me
    change_me = True
    return item
"""

    workflow_code = action_manifest["workflow_code"]
    script = (
        workflow_code
        + "\n__run_result=workflow(settings=dict("
        " {target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads, "change_me": False}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is False
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)

    campaign_id = "xxxx_withFormat"

    with_format_function = workflow_code.replace(
        action_manifest["workflow_code_format_placeholder"], format_function
    )
    script = (
        with_format_function
        + "\n__run_result=workflow(settings=dict("
        " {target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is True
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)


def test_pull_workflow_code_with_logics(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][0]
    assert action_manifest["name"] == "pull_leads"

    campaign_id = "xxxx_4652"
    logics_functions = """
def logic(item):
    global change_me
    change_me = True
    return item
logics = [logic]
"""

    workflow_code = action_manifest["workflow_code"]
    # FIXME this should work without needing to supply dummy_str
    script = (
        workflow_code
        + "\n__run_result=workflow(settings=dict("
        " {target_prefix}campaign_id='{campaign_id}',"
        " {target_prefix}dummy_str='FIXME'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads, "change_me": False}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is False
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)

    campaign_id = "xxxx_withLogics"

    with_format_function = workflow_code.replace(
        action_manifest["workflow_code_logics_placeholder"], logics_functions
    )
    script = (
        with_format_function
        + "\n__run_result=workflow(settings=dict("
        " {target_prefix}campaign_id='{campaign_id}',"
        " {target_prefix}dummy_str='FIXME'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is True
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)


def test_catch_workflow_code(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][1]
    assert action_manifest["name"] == "catch_user"

    campaign_id = "xxxx_356"
    n_males = len([u for u in USERS_DB if u["gender"] is Gender.male])

    workflow_code = action_manifest["workflow_code"]
    script = (
        workflow_code
        + "\n__run_result=workflow(_request=dict(gender='male'),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    result = global_namespace["__run_result"]
    assert result.status is Status.success
    assert result.reason is Reason.none
    assert result.events[Event.read_success] == n_males
    assert result.events[Event.format_failure] == 0
    assert result.events[Event.logics_failure] == 0
    assert result.events[Event.logics_discard] == 0
    assert result.events[Event.write_failure] == 0
    assert len(LEADS_DB[campaign_id]) == n_males


def test_catch_workflow_code_with_format(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][1]
    assert action_manifest["name"] == "catch_user"

    campaign_id = "xxxx_346"
    format_function = """
def format(item):
    global change_me
    change_me = True
    return item
"""

    workflow_code = action_manifest["workflow_code"]
    script = (
        workflow_code
        + "\n__run_result=workflow(_request=dict(),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads, "change_me": False}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is False
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)

    campaign_id = "yyy_withFormat"

    with_format_function = workflow_code.replace(
        action_manifest["workflow_code_format_placeholder"], format_function
    )
    script = (
        with_format_function
        + "\n__run_result=workflow(_request=dict(),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is True
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)


def test_catch_workflow_code_with_logics(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][1]
    assert action_manifest["name"] == "catch_user"

    campaign_id = "xxxx_874"
    logics_functions = """
def logic(item):
    global change_me
    change_me = True
    return item
logics = [logic]
"""

    workflow_code = action_manifest["workflow_code"]
    script = (
        workflow_code
        + "\n__run_result=workflow(_request=dict(),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads, "change_me": False}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is False
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)

    campaign_id = "yyy_withLogics"

    with_format_function = workflow_code.replace(
        action_manifest["workflow_code_logics_placeholder"], logics_functions
    )
    script = (
        with_format_function
        + "\n__run_result=workflow(_request=dict(),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    assert global_namespace["__run_result"].status is Status.success
    assert global_namespace["change_me"] is True
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)


def test_catch_workflow_code_with_event_parser(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][1]
    assert action_manifest["name"] == "catch_user"

    campaign_id = "xxxx_6876"
    n_males = len([u for u in USERS_DB if u["gender"] is Gender.male])
    event_parser = """
def event_parser(event):
    return dict(gender=event["desired_gender"])
"""

    workflow_code = action_manifest["workflow_code"]
    # 'desired_gender' should not have any effect
    script = (
        workflow_code
        + "\n__run_result=workflow(_request=dict(desired_gender='male'),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    result = global_namespace["__run_result"]
    assert result.status is Status.success
    assert result.events[Event.read_success] == len(USERS_DB)
    assert len(LEADS_DB[campaign_id]) == len(USERS_DB)

    campaign_id = "xxxx_withEventParser"
    with_event_parser = workflow_code.replace(
        action_manifest["workflow_code_event_parser_placeholder"], event_parser
    )
    script = (
        with_event_parser
        + "\n__run_result=workflow(_request=dict(desired_gender='male'),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    result = global_namespace["__run_result"]
    assert result.status is Status.success
    assert result.events[Event.read_success] == n_males
    assert len(LEADS_DB[campaign_id]) == n_males


def test_catch_workflow_code_with_event_parser_failure(with_smartleads):
    action_manifest = SmartLeads.manifest()["actions"][1]
    assert action_manifest["name"] == "catch_user"

    campaign_id = "xxxx_43434"
    event_parser = """
def event_parser(event):
    raise Exception()
"""

    workflow_code = action_manifest["workflow_code"]
    with_event_parser = workflow_code.replace(
        action_manifest["workflow_code_event_parser_placeholder"], event_parser
    )
    script = (
        with_event_parser
        + "\n__run_result=workflow(_request=dict(desired_gender='male'),"
        " settings=dict({target_prefix}campaign_id='{campaign_id}'))".format(
            target_prefix=action_manifest["workflow_code_target_settings_prefix"],
            campaign_id=campaign_id,
        )
    )
    global_namespace = {"hrflow_connectors": with_smartleads}

    assert len(LEADS_DB[campaign_id]) == 0

    exec(script, global_namespace)

    result = global_namespace["__run_result"]
    assert result.status is Status.fatal
    assert result.reason is Reason.event_parsing_failure
    assert result.events[Event.read_success] == 0
    assert len(LEADS_DB[campaign_id]) == 0
