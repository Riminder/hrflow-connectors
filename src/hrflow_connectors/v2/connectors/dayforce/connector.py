import typing as t

from hrflow_connectors.v2.connectors.dayforce.warehouse import DayforceWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_job(dayforce_job: t.Dict) -> t.Dict:
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
    job["name"] = dayforce_job.get("Title")
    job["summary"] = dayforce_job.get("Description")
    job["reference"] = str(dayforce_job.get("ReferenceNumber"))
    job["url"] = dayforce_job.get("JobDetailsUrl")

    # location
    job["location"] = dict(
        text=dayforce_job.get("AddressLine1"),
        lat=None,
        lng=None,
        fields=dict(
            city=dayforce_job.get("City"),
            state=dayforce_job.get("State"),
            country=dayforce_job.get("Country"),
            postcode=dayforce_job.get("PostalCode"),
        ),
    )

    # sections
    job["sections"] = [
        dict(
            name="dayforce_description",
            title="dayforce_description",
            description=dayforce_job.get("Description"),
        )
    ]
    job["created_at"] = dayforce_job.get("DatePosted")
    job["updated_at"] = dayforce_job.get("LastUpdated")

    # tags
    job["tags"] = [
        dict(
            name="dayforce_parent_requisition_code",
            value=dayforce_job.get("ParentRequisitionCode"),
        ),
        dict(name="dayforce_apply_url", value=dayforce_job.get("ApplyUrl")),
        dict(
            name="dayforce_client-site-name", value=dayforce_job.get("ClientSiteName")
        ),
        dict(
            name="dayforce_client-site-ref-code",
            value=dayforce_job.get("ClientSiteXRefCode"),
        ),
        dict(name="dayforce_company_name", value=dayforce_job.get("CompanyName")),
        dict(name="dayforce_remote", value=dayforce_job.get("IsVirtualLocation")),
    ]

    return job


def format_archive(dayforce_job: t.Dict) -> t.Dict:
    return dict(reference=str(dayforce_job.get("ReferenceNumber")))


DESCRIPTION = (
    "Dayforce enterprise, formerly Ceridian, HCM software combines payroll, HR,"
    " benefits, talent and workforce management in a single cloud application to power"
    " the future of work."
)

Dayforce = Connector(
    name="Dayforce",
    type=ConnectorType.HCM,
    subtype="dayforce",
    description=DESCRIPTION,
    url="https://www.dayforce.com/",
    warehouse=DayforceWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.archive, Entity.job, Direction.inbound, format=format_archive),
    ),
)
