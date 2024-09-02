import typing as t

from hrflow_connectors.connectors.ceridian.warehouse import CeridianJobWarehouse
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

from .schemas import CeridianDayforceJobModel


def format_job(data: CeridianDayforceJobModel) -> t.Dict:
    """
    format a job into the hrflow job object format
    Args:
        data (CeridianDayforceJobModel): a job object pulled from a ceridian
        dayforce job feed space
    Returns:
        HrflowJob: a job into the hrflow job object format
    """
    job = dict()

    # basic information
    job["name"] = data.get("Title")
    job["summary"] = None
    job["reference"] = str(data.get("ReferenceNumber")) + str(
        data.get("ParentRequisitionCode")
    )
    job["url"] = data.get("JobDetailsUrl")

    # location
    location = data.get("City")
    state = data.get("State")
    country = data.get("Country")
    postal_code = data.get("PostalCode")
    geojson = dict(state=state, country=country, postal_code=postal_code)
    job["location"] = dict(text=location, lat=None, lng=None, geojson=geojson)

    # sections
    description = data.get("Description")
    job["sections"] = [
        dict(
            name="dayforce_description",
            title="dayforce_description",
            description=description,
        )
    ]
    job["created_at"] = data.get("DatePosted")
    job["updated_at"] = data.get("LastUpdated")

    # tags
    apply_url = str(data.get("ApplyUrl"))
    client_site_name = str(data.get("ClientSiteName"))
    client_site_ref_code = str(data.get("ClientSiteXRefCode"))
    company_name = str(data.get("CompanyName"))
    remote = str(data.get("IsVirtualLocation"))
    job["tags"] = [
        dict(name="dayforce_apply_url", value=apply_url),
        dict(name="dayforce_client-site-name", value=client_site_name),
        dict(name="dayforce_client-site-ref-code", value=client_site_ref_code),
        dict(name="dayforce_company_name", value=company_name),
        dict(name="dayforce_remote", value=remote),
    ]

    return job


DESCRIPTION = (
    "Dayforce enterprise HCM software combines payroll, HR, benefits, talent and"
    " workforce management in a single cloud application to power the future of"
    " work.Ceridian"
)

Ceridian = Connector(
    name="Ceridian",
    type=ConnectorType.HCM,
    description=DESCRIPTION,
    url="https://www.ceridian.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***Ceridian*** API and send them"
                " to an ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=CeridianJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
