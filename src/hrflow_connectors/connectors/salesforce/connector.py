import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.salesforce.warehouse import (
    SalesforceJobsWarehouse,
    SalesforceProfileWarehouse,
)
from hrflow_connectors.core import BaseActionParameters, Connector, ConnectorAction


def format(profile_or_job: t.Dict) -> t.Dict:
    return profile_or_job


Salesforce = Connector(
    name="Salesforce",
    description="Description yet to write",
    url="https://www.salesforce.com",
    actions=[
        ConnectorAction(
            name="push_jobs",
            description=(
                "Retrieves jobs from HrFlow.ai Board and posts them to Salesforce API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format
            ),
            origin=HrFlowJobWarehouse,
            target=SalesforceJobsWarehouse,
        ),
        ConnectorAction(
            name="push_profile",
            description=("Writes a profile from HrFlow.ai Source to Salesforce API",),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format
            ),
            origin=HrFlowProfileWarehouse,
            target=SalesforceProfileWarehouse,
        ),
    ],
)
