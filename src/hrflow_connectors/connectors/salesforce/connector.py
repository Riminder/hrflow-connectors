import typing as t

<<<<<<< HEAD
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.salesforce.warehouse import (
    SalesforceJobsWarehouse,
    SalesforceProfileWarehouse,
)
from hrflow_connectors.core import BaseActionParameters, Connector, ConnectorAction
=======
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileWarehouse
from hrflow_connectors.connectors.salesforce.schemas import HrFlowProfile
from hrflow_connectors.connectors.salesforce.warehouse import SalesforceProfileWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
>>>>>>> 8bcf90301e0fe51739192f996dfe740420d12527


def format(profile_or_job: t.Dict) -> t.Dict:
    return profile_or_job


Salesforce = Connector(
    name="Salesforce",
    description="Description yet to write",
    url="https://www.salesforce.com",
    actions=[
        ConnectorAction(
<<<<<<< HEAD
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
=======
            name="push_profile",
            data_schema=HrFlowProfile,
            description="Writes a profile from HrFlow.ai Source to Salesforce API",
>>>>>>> 8bcf90301e0fe51739192f996dfe740420d12527
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format
            ),
            origin=HrFlowProfileWarehouse,
            target=SalesforceProfileWarehouse,
<<<<<<< HEAD
=======
            trigger_type=WorkflowType.catch,
>>>>>>> 8bcf90301e0fe51739192f996dfe740420d12527
        ),
    ],
)
