from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.talentlink.warehouse import (
    TalentLinkJobWarehouse,
    TalentLinkProfileWarehouse,
)
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

DESCRIPTION = (
    "TalentLink is Cornerstone's most effective platform for managing talent acquisition processes."
    " Take full control of your recruitment processes and involve your teams in the decision-making process."
)

TalentLink = Connector(
    name="TalentLink",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://emea3.mrtedtalentlink.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***TalentLink*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=TalentLinkJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all profiles from TalentLink and sends them to an"
                " Hrflow.ai Source."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilesActionParameters", format=format_tl_profile
            ),
            origin=TalentLinkProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source to TalentLink via the API"
                " for the given `job_id`."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_hrflow_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=TalentLinkProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
