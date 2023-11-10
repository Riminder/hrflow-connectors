import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.jazzhr.warehouse import (
    JazzhrApplicantWarehouse,
    JazzhrJobWarehouse,
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


def format_jazzhr_job(jazzhr_job: t.Dict) -> t.Dict:
    hrflow_job = {}
    location_text = (
        f"{jazzhr_job['city']}, {jazzhr_job['state']}, {jazzhr_job['country']}"
    )
    hrflow_job["name"] = jazzhr_job["title"]
    hrflow_job["description"] = jazzhr_job["description"]
    hrflow_job["location"] = {"text": location_text, "lat": None, "lng": None}
    hrflow_job["sections"] = [
        {
            "name": "Job Description",
            "title": "Job Description",
            "description": jazzhr_job["description"],
        }
    ]
    hrflow_job["ranges_float"] = [
        {
            "name": "Salary",
            "value_min": jazzhr_job["approved_salary_range_minimum"],
            "value_max": jazzhr_job["approved_salary_range_maximum"],
            "unit": None,
        }
    ]
    hrflow_job["summary"] = jazzhr_job["job_notes"]
    tags_list = [
        "hiring_lead_id",
        "employment_type",
        "minimum_experience",
        "department",
        "job_status",
        "syndication",
        "workflow_id",
        "custom_questions_id",
        "internal_job_code",
        "eeo_1_job_category",
        "eeo_1_job_category",
        "open_date",
    ]
    for tag in tags_list:
        if jazzhr_job[tag] is not None:
            hrflow_job["tags"].append(jazzhr_job[tag])
    return hrflow_job


def format_jazzhr_applicant(jazzhr_applicant: t.Dict) -> t.Dict:
    hrflow_profile = {}
    location_text = (
        f"{jazzhr_applicant['postal']}, {jazzhr_applicant['city']},"
        f" {jazzhr_applicant['state']}, {jazzhr_applicant['country']}"
    )
    hrflow_profile["info"]["first_name"] = jazzhr_applicant["first_name"]
    hrflow_profile["info"]["last_name"] = jazzhr_applicant["last_name"]
    hrflow_profile["info"]["email"] = jazzhr_applicant["email"]
    hrflow_profile["info"]["phone"] = jazzhr_applicant["phone"]
    hrflow_profile["info"]["location"] = {
        "text": location_text,
        "lat": None,
        "lng": None,
    }
    list_of_sites = ["linkedin", "website", "twitter"]
    hrflow_profile["info"]["urls"] = []
    for site in list_of_sites:
        if jazzhr_applicant[site] is not None:
            hrflow_profile["info"]["urls"].append(
                {
                    "type": site,
                    "url": jazzhr_applicant[site],
                }
            )
    hrflow_profile["info"]["gender"] = ""
    if jazzhr_applicant["eeo_gender"] == 1:
        hrflow_profile["info"]["gender"] = "female"
    elif jazzhr_applicant["eeo_gender"] == 2:
        hrflow_profile["info"]["gender"] = "male"
    hrflow_profile["text"] = jazzhr_applicant["resume_text"]
    list_of_tags = (
        [
            "apply_date",
            "address",
            "job",
            "job_status",
            "workflow_id",
            "coverletter",
            "source",
            "referral",
            "license",
            "cdl",
            "relocate",
            "citizen",
            "education",
            "college",
            "gpa",
            "over18",
            "flighthours",
            "flightgrade",
            "felony",
            "felonyexplain",
            "custom_questions_id",
            "internal_job_code",
            "eeo_1_job_category",
            "open_date",
            "base64_resume",
        ],
    )
    hrflow_profile["tags"] = []
    for tag in list_of_tags:
        if jazzhr_applicant[tag] is not None:
            hrflow_profile["tags"].append(jazzhr_applicant[tag])
    hrflow_profile["skills"] = []
    hrflow_profile["experiences"] = []
    hrflow_profile["educations"] = []
    hrflow_profile["languages"] = []
    hrflow_profile["certifications"] = []
    hrflow_profile["interests"] = []
    return hrflow_profile


DESCRIPTION = (
    "JazzHR is a powerful, user-friendly, applicant tracking system "
    "that is purpose-built to help businesses exceed their recruiting goals."
)

Jazzhr = Connector(
    name="Jazzhr",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.jazzhr.com",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves Applicants from Jazzhr "
                " and writes them to an Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_jazzhr_applicant
            ),
            origin=JazzhrApplicantWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from Jazzhr and writes them to an Hrflow.ai board"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_jazzhr_job
            ),
            origin=JazzhrJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
