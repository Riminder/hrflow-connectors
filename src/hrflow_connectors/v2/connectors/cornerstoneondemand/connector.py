from hrflow_connectors.v2.connectors.cornerstoneondemand.warehouse import (
    CornerstoneOnDemandWarehouse,
)
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_job_tags(job_requisition: dict) -> list[dict]:
    t = lambda name, value: {"name": name, "value": value}
    return [
        t("ApplicantCount", job_requisition.get("ApplicantCount")),
        t("Compensation", job_requisition.get("Compensation")),
        t("ContactPhone", job_requisition.get("ContactPhone")),
        t("DefaultEffectiveDate", job_requisition.get("DefaultEffectiveDate")),
        t("DefaultExpirationDate", job_requisition.get("DefaultExpirationDate")),
        t("Division", job_requisition.get("Division")),
        t("EmploymentType", job_requisition.get("EmploymentType")),
        t("HiringManager", job_requisition.get("HiringManager", {}).get("Name")),
        t("IdealQualification", job_requisition.get("IdealQualification")),
        t("Ongoing", job_requisition.get("Ongoing")),
        t("Priority", job_requisition.get("Priority")),
        t("Ref", job_requisition.get("Ref")),
        t("TargetHireDate", job_requisition.get("TargetHireDate")),
    ]


def format_job_requisition(job_requisition: dict) -> dict:
    hrflow_job = dict(
        reference=job_requisition.get("Id"),
        name=job_requisition.get("Title"),
        url=job_requisition.get("CareerSites", [{}])[0].get("URL"),
        created_at=job_requisition.get("CreateDateLocal"),
        updated_at=job_requisition.get("LastModificationDate"),
        summary=job_requisition.get("ExternalDescription"),
        location=dict(
            text=job_requisition.get("Address"),
            lat=None,
            lng=None,
            fields=dict(
                city=job_requisition.get("AddressDetails", {}).get("City"),
                state=job_requisition.get("AddressDetails", {}).get("State"),
                country=job_requisition.get("AddressDetails", {}).get("Country"),
                postcode=job_requisition.get("AddressDetails", {}).get("PostalCode"),
            ),
        ),
        responsibilities=", ".join(job_requisition.get("JobResponsibilities", [])),
        requirements=job_requisition.get("MinimumQualification"),
        ranges_float=[
            dict(
                name="Salary Range",
                min=job_requisition.get("RangeLow"),
                max=job_requisition.get("RangeHigh"),
                unit=job_requisition.get("CurrencySymbol")
                or job_requisition.get("Currency"),
            )
        ],
        tags=get_job_tags(job_requisition),
    )
    return hrflow_job


def format_item_for_archive(item: dict) -> dict:
    return dict(reference=item.get("Id"))


def get_profile_tags(job_applicant: dict) -> list[dict]:
    t = lambda name, value: {"name": name, "value": value}
    return [
        t("ApplicantHireDate", job_applicant.get("ApplicantHireDate")),
        t("AverageRating", job_applicant.get("AverageRating")),
        t("CandidateType", job_applicant.get("CandidateType")),
        t("CostCenter", job_applicant.get("CostCenterTitle")),
        t("Division", job_applicant.get("DivisionTitle")),
        t("Ethnicity", job_applicant.get("Ethnicity")),
        t("Grade", job_applicant.get("GradeTitle")),
        t("JobRequisitionId", job_applicant.get("JobRequisitionId")),
        t("Position", job_applicant.get("PositionTitle")),
        t("PreviousStatus", job_applicant.get("PreviousStatus")),
        t("RequisitionId", job_applicant.get("RequisitionId")),
        t("RequisitionName", job_applicant.get("RequisitionName")),
        t("Source", job_applicant.get("Source")),
        t("Status", job_applicant.get("Status")),
        t("UserId", job_applicant.get("UserId")),
        t("Username", job_applicant.get("Username")),
    ]


def format_job_applicant(job_applicant: dict) -> dict:
    hrflow_profile = dict(
        reference=job_applicant.get("Id"),
        created_at=job_applicant.get("ApplicationReceivedDateLocal"),
        info=dict(
            first_name=job_applicant.get("FirstName"),
            last_name=job_applicant.get("LastName"),
            full_name=job_applicant.get("Name"),
            email=job_applicant.get("Email"),
            phone=job_applicant.get("Phone"),
            location=dict(
                text=", ".join(
                    [
                        job_applicant.get("AddressLine1", ""),
                        job_applicant.get("AddressLine2", ""),
                    ]
                ).strip(),
                fields=dict(
                    city=job_applicant.get("City"),
                    state=job_applicant.get("State"),
                    country=job_applicant.get("Country"),
                    postcode=job_applicant.get("PostalCode"),
                ),
            ),
            gender=job_applicant.get("Gender"),
            picture=job_applicant.get("ThumbImgUrl"),
        ),
        experiences=[],
        educations=[],
        skills=[],
        tags=get_profile_tags(job_applicant),
    )
    return hrflow_profile


def format_hrflow_profile(hrflow_profile: dict) -> dict:
    hrflow_info = hrflow_profile["info"]
    resume_url = next(
        (
            attachment
            for attachment in hrflow_profile.get("attachments", [])
            if attachment.get("type") == "resume"
        ),
        {},
    ).get("public_url")

    candidate = dict(
        email=hrflow_info["email"],
        firstName=hrflow_info["first_name"],
        lastName=hrflow_info["last_name"],
        contactDetails=dict(
            address1=hrflow_info["location"]["text"],
            city=hrflow_info["location"].get("fields", {}).get("city"),
            state=hrflow_info["location"].get("fields", {}).get("state"),
            country=hrflow_info["location"].get("fields", {}).get("country"),
            postalCode=hrflow_info["location"].get("fields", {}).get("postcode"),
            PhoneNumber=hrflow_info["phone"],
        ),
        resume=resume_url,
    )
    return candidate


DESCRIPTION = (
    "Cornerstone powers the potential of organizations and their people to thrive in a"
    " changing world. Cornerstone Galaxy, the complete AI-powered workforce agility"
    " platform, meets organizations where they are. With Galaxy, organizations can"
    " identify skills gaps and development opportunities, retain and engage top talent,"
    " and provide multimodal learning experiences to meet the diverse needs of the"
    " modern workforce. More than 7,000 organizations and 140 million users in 186"
    " countries use Cornerstone Galaxy to build high-performing, future-ready"
    " organizations and people today."
)

CornerstoneOnDemand = Connector(
    name="Cornerstone OnDemand",
    type=ConnectorType.ATS,
    subtype="cornerstoneondemand",
    description=DESCRIPTION,
    url="https://www.cornerstoneondemand.com/",
    warehouse=CornerstoneOnDemandWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job_requisition),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job_requisition),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_item_for_archive
        ),
        Flow(
            Mode.create, Entity.profile, Direction.inbound, format=format_job_applicant
        ),
        Flow(
            Mode.update, Entity.profile, Direction.inbound, format=format_job_applicant
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_item_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
    ),
)
