import datetime
import re
from typing import Any, Dict, List, Optional

from hrflow_connectors.v2.connectors.sapsuccessfactors.warehouse import SAPWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def convert_sap_date_timestamp(date_str: str) -> Optional[str]:
    match = re.search(r"\d+", date_str)
    if match:
        timestamp = match.group()
        return datetime.datetime.fromtimestamp(int(timestamp) / 1000).isoformat()
    else:
        return None


def format_sap_job_location(sap_job_requisition: Dict[str, Any]) -> Dict[str, Any]:
    """
    Format SAP job location to Hrflow location
    Args:
        sap_job_requisition (Dict[str, Any]): SAP job object
    Returns:
        Dict[str, Any]: Hrflow location
    """
    job_location_obj = sap_job_requisition.get("location_obj", {})
    if job_location_obj and job_location_obj["results"]:
        job_location = job_location_obj["results"][0]
        return dict(
            text=job_location.get("name"),
            lat=None,
            lng=None,
            fields=dict(
                country=sap_job_requisition.get("country"),
                world_region=job_location.get("locationGroup"),
            ),
        )
    return dict(text=None, lat=None, lng=None)


create_tag = lambda name, value: dict(name=name, value=value)


def format_sap_job(sap_job_requisition: Dict) -> Dict:
    """
    Format retrieved jobs into a HrFlow job object
    Args:
        data (SAPSuccessFactorsJob): job to format
    Returns:
        HrflowJob: job in the HrFlow job object format
    """

    job_requisition_locale = sap_job_requisition.get("jobReqLocale", {}).get(
        "results", [{}]
    )[0]

    hrflow_job = dict(
        reference=sap_job_requisition.get("jobReqId"),
        name=job_requisition_locale.get("jobTitle"),
        location=format_sap_job_location(sap_job_requisition),
        url=sap_job_requisition.get("quickApply"),
        created_at=(
            convert_sap_date_timestamp(sap_job_requisition["createdDateTime"])
            if sap_job_requisition.get("createdDateTime")
            else None
        ),
        updated_at=(
            convert_sap_date_timestamp(sap_job_requisition["lastModifiedDateTime"])
            if sap_job_requisition.get("lastModifiedDateTime")
            else None
        ),
        summary=job_requisition_locale.get("jobDescription"),
        skills=[
            dict(
                name=skill.get("name"),
                value=None,
                type=None,
            )
            for skill in sap_job_requisition.get("competencies", {"results": []}).get(
                "results"
            )
        ],
        requirements=sap_job_requisition.get("jobProfile", {}).get("longDesc_en_US"),
        tags=[
            create_tag(
                "status",
                sap_job_requisition.get("status", {"results": [{}]})["results"][0].get(
                    "status"
                ),
            ),
            create_tag(
                "department",
                sap_job_requisition.get("department_obj", {}).get("name_en_US"),
            ),
            create_tag(
                "division",
                sap_job_requisition.get("division_obj", {}).get("name_en_US"),
            ),
            create_tag("facility", sap_job_requisition.get("facility")),
            create_tag(
                "legal_entity",
                sap_job_requisition.get("legalEntity_obj", {}).get("name_en_US"),
            ),
            create_tag(
                "job_role_entity",
                sap_job_requisition.get("jobRoleEntity", {}).get("name_en_US"),
            ),
            create_tag(
                "recruiter_team",
                (
                    sap_job_requisition["recruiterTeamGroup"]["results"][0].get(
                        "userGroupName"
                    )
                    if sap_job_requisition.get("recruiterTeamGroup", {"results": [{}]})[
                        "results"
                    ]
                    else None
                ),
            ),
            create_tag(
                "hire_manager_team",
                (
                    sap_job_requisition["hiringManagerTeamGroup"]["results"][0].get(
                        "userGroupName"
                    )
                    if sap_job_requisition.get(
                        "hiringManagerTeamGroup", {"results": [{}]}
                    )["results"]
                    else None
                ),
            ),
            create_tag(
                "sourcer_team",
                (
                    sap_job_requisition["sourcerTeamGroup"]["results"][0].get(
                        "userGroupName"
                    )
                    if sap_job_requisition.get("sourcerTeamGroup", {"results": [{}]})[
                        "results"
                    ]
                    else None
                ),
            ),
            create_tag(
                "corporate_posting", sap_job_requisition.get("corporatePosting")
            ),
            create_tag("cost_of_hire", sap_job_requisition.get("costOfHire")),
            create_tag("number_openings", sap_job_requisition.get("numberOpenings")),
            create_tag("openings_filled", sap_job_requisition.get("openingsFilled")),
            create_tag("salary_base", sap_job_requisition.get("salaryBase")),
            create_tag("work_hours", sap_job_requisition.get("workHours")),
            create_tag("time_to_fill", sap_job_requisition.get("timeToFill")),
        ],
        ranges_float=[
            dict(
                name="salary_range",
                value_min=sap_job_requisition.get("salaryMin"),
                value_max=sap_job_requisition.get("salaryMax"),
                unit=sap_job_requisition.get("currency"),
            )
        ],
    )

    return hrflow_job


def format_sap_job_for_archive_in_hrflow(sap_job_requisition: Dict) -> Dict:
    return dict(reference=sap_job_requisition["jobReqId"])


def format_sap_candidate_experiences(candidate_data: Dict) -> List:
    experiences = [
        dict(
            company=experience.get("employer"),
            title=None,
            description=None,
            location=dict(
                text=None,
                lat=None,
                lng=None,
            ),
            date_start=convert_sap_date_timestamp(experience.get("startDate")),
            date_end=convert_sap_date_timestamp(experience.get("endDate")),
        )
        for experience in candidate_data.get("outsideWorkExperience", {"results": []})[
            "results"
        ]
    ]

    experiences.extend(
        [
            dict(
                company=experience.get("department"),
                title=experience.get("title"),
                description=None,
                location=dict(
                    text=None,
                    lat=None,
                    lng=None,
                ),
                date_start=convert_sap_date_timestamp(experience.get("startDate")),
                date_end=convert_sap_date_timestamp(experience.get("endDate")),
            )
            for experience in candidate_data.get(
                "insideWorkExperience", {"results": []}
            )["results"]
        ]
    )
    return experiences


def format_sap_candidate(candidate_data: Dict) -> Dict:
    hrflow_profile = dict(
        reference=candidate_data["candidateId"],
        info=dict(
            first_name=candidate_data["firstName"],
            last_name=candidate_data["lastName"],
            full_name=f"{candidate_data['firstName']} {candidate_data['lastName']}",
            email=candidate_data["contactEmail"],
            phone=candidate_data["cellPhone"],
            location=dict(
                text=candidate_data["address"],
                lat=None,
                lng=None,
                fields=dict(
                    country=candidate_data["country"],
                    city=candidate_data["city"],
                    postcode=candidate_data["zip"],
                ),
            ),
        ),
        created_at=(
            convert_sap_date_timestamp(candidate_data["creationDateTime"])
            if candidate_data.get("creationDateTime")
            else None
        ),
        updated_at=(
            convert_sap_date_timestamp(candidate_data["lastModifiedDateTime"])
            if candidate_data.get("lastModifiedDateTime")
            else None
        ),
        educations=[
            dict(
                school=education.get("school"),
                title=None,
                description=None,
                location=dict(text=None, lat=None, lng=None),
                date_start=convert_sap_date_timestamp(education.get("startDate")),
                date_end=convert_sap_date_timestamp(education.get("endDate")),
            )
            for education in candidate_data.get("education", {"results": []})["results"]
        ],
        experiences=format_sap_candidate_experiences(candidate_data),
        skills=[],
        resume=dict(
            raw=candidate_data["resume"],
        ),
        tags=[
            create_tag(
                "consent_to_marketing", candidate_data.get("consentToMarketing")
            ),
            create_tag("current_title", candidate_data.get("currentTitle")),
            create_tag(
                "date_of_availability",
                (
                    convert_sap_date_timestamp(candidate_data["dateofAvailability"])
                    if candidate_data.get("dateofAvailability")
                    else None
                ),
            ),
            create_tag(
                "last_login_date_time",
                (
                    convert_sap_date_timestamp(candidate_data["lastLoginDateTime"])
                    if candidate_data.get("lastLoginDateTime")
                    else None
                ),
            ),
            create_tag("visibility_option", candidate_data.get("visibilityOption")),
        ],
    )

    return hrflow_profile


def format_sap_candidate_for_archive_in_hrflow(candidate_data: Dict) -> Dict:
    return dict(reference=candidate_data["candidateId"])


def date_to_sap_timestamp(date_str: str) -> str:
    dt = datetime.datetime.fromisoformat(date_str)
    timestamp = int(dt.timestamp() * 1000)
    return f"/Date({timestamp})/"


def format_hrflow_profile(hrflow_profile: Dict) -> Dict:
    """
    Formats an hrflow profile into a sap successfactors candidate
    Args:
        hrflow_profile (HrflowProfile): profile to format
    Returns:
        SapCandidateModel: a SAP successfactors profile
    """
    info = hrflow_profile.get("info", {})
    location = info.get("location", {})
    fields = location.get("fields", {})

    sap_profile = dict(
        firstName=info.get("first_name"),
        lastName=info.get("last_name"),
        primaryEmail=info.get("email"),
        cellPhone=info.get("phone"),
        address=location.get("text"),
        country=fields["country"][:2] if fields.get("country") else None,
        city=fields.get("city"),
        zip=fields.get("postcode"),
        education=(
            dict(
                results=[
                    dict(
                        school=education.get("school"),
                        startDate=(
                            date_to_sap_timestamp(education["date_start"])
                            if education.get("date_start")
                            else None
                        ),
                        endDate=(
                            date_to_sap_timestamp(education["date_end"])
                            if education.get("date_end")
                            else None
                        ),
                    )
                    for education in hrflow_profile.get("educations", [])
                ]
            )
            if hrflow_profile.get("educations")
            else dict(results=[])
        ),
        outsideWorkExperience=(
            dict(
                results=[
                    dict(
                        employer=experience.get("company"),
                        startDate=(
                            date_to_sap_timestamp(experience["date_start"])
                            if experience.get("date_start")
                            else None
                        ),
                        endDate=(
                            date_to_sap_timestamp(experience["date_end"])
                            if experience.get("date_end")
                            else None
                        ),
                    )
                    for experience in hrflow_profile.get("experiences", [])
                ]
            )
            if hrflow_profile.get("experiences")
            else dict(results=[])
        ),
    )

    return sap_profile


def format_hrflow_profile_for_update(hrflow_profile: Dict) -> Dict:
    info = hrflow_profile.get("info", {})
    location = info.get("location", {})
    fields = location.get("fields", {})

    sap_profile = dict(
        candidateId=hrflow_profile["reference"],
        firstName=info.get("first_name"),
        lastName=info.get("last_name"),
        cellPhone=info.get("phone"),
        address=location.get("text"),
        country=fields["country"][:2] if fields.get("country") else None,
        city=fields.get("city"),
        zip=fields.get("postcode"),
    )
    return sap_profile


DESCRIPTION = (
    "SAP SuccessFactors HCM is a suite of cloud-based HCM software applications that"
    " supports core HR and payroll, talent management, HR analytics and workforce"
    " planning, and employee experience management."
)

SAPSuccessFactors = Connector(
    name="SAP SuccessFactors",
    type=ConnectorType.HCM,
    subtype="sapsuccessfactors",
    description=DESCRIPTION,
    url="https://www.sap.com/",
    warehouse=SAPWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_sap_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_sap_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_sap_job_for_archive_in_hrflow,
        ),
        Flow(
            Mode.create, Entity.profile, Direction.inbound, format=format_sap_candidate
        ),
        Flow(
            Mode.update, Entity.profile, Direction.inbound, format=format_sap_candidate
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_sap_candidate_for_archive_in_hrflow,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update,
        ),
    ),
)
