import typing as t

from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.workday.warehouse import (
    WorkdayJobsWarehouse,
    WorkdayProfilesWarehouse,
)

_WORKDAY_HCM_URL = (
    "https://www.workday.com/en-us/products/human-capital-management/overview.html"
)
_WORKDAY_DESCRIPTION = (
    "Manage the full talent acquisition lifecycle. With recruiting, engagement, and"
    " Workday Skills Cloud unified with Workday Human Capital Management (HCM), we've"
    " got you covered every step of the way. Our skills intelligence foundation helps"
    " you build diverse teams by expanding candidate pools with equitable, AI- and"
    " ML-driven job recommendations."
)


def _format_workday_job(workday_job: t.Dict) -> t.Dict:
    pass


def _format_hrflow_profile(workday_profile: t.Dict) -> t.Dict:
    pass


Workday = Connector(
    name="Workday",
    type=ConnectorType.HCM,
    description=_WORKDAY_DESCRIPTION,
    url=_WORKDAY_HCM_URL,
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=WorkdayJobsWarehouse,
            target=HrFlowJobWarehouse,
            description="Retrive Jobs from Workday and index them to a HrFlow board.",
            parameters=BaseActionParameters.with_defaults(
                "WorkdayReadJobsParameters", format=_format_workday_job
            ),
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            action_type=ActionType.outbound,
            origin=HrFlowProfileWarehouse,
            target=WorkdayProfilesWarehouse,
            description="Retrive profile from HrFlow source and post them to Workday.",
            parameters=BaseActionParameters.with_defaults(
                "WorkdayWriteProfileParameters", format=_format_hrflow_profile
            ),
        ),
    ],
)
