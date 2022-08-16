import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileWarehouse
from hrflow_connectors.connectors.salesforce.schemas import HrFlowProfile
from hrflow_connectors.connectors.salesforce.warehouse import SalesforceProfileWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def format(profile_or_job: t.Dict) -> t.Dict:
    return profile_or_job


Salesforce = Connector(
    name="Salesforce",
    description="Description yet to write",
    url="https://www.salesforce.com",
    actions=[
        ConnectorAction(
            name="push_profile",
            data_schema=HrFlowProfile,
            description="Writes a profile from HrFlow.ai Source to Salesforce API",
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format
            ),
            origin=HrFlowProfileWarehouse,
            target=SalesforceProfileWarehouse,
            trigger_type=WorkflowType.catch,
        ),
    ],
)
