import base64
import typing as t

import requests

from hrflow_connectors.v2.connectors.jazzhr.warehouse import JazzhrWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_jazzhr_job(jazzhr_job: t.Dict) -> t.Dict:
    hrflow_job = {}
    location_text = (
        f"{jazzhr_job['city']}, {jazzhr_job['state']}, {jazzhr_job['country']}"
    )
    hrflow_job["reference"] = jazzhr_job["id"]
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
            hrflow_job["tags"].append(dict(name=tag, value=jazzhr_job[tag]))
    return hrflow_job


def format_jazzhr_applicant(jazzhr_applicant: t.Dict) -> t.Dict:
    hrflow_profile = {}
    location_text = (
        f"{jazzhr_applicant['postal']}, {jazzhr_applicant['city']},"
        f" {jazzhr_applicant['state']}, {jazzhr_applicant['country']}"
    )
    hrflow_profile["reference"] = jazzhr_applicant["id"]
    hrflow_profile["info"]["first_name"] = jazzhr_applicant["first_name"]
    hrflow_profile["info"]["last_name"] = jazzhr_applicant["last_name"]
    hrflow_profile["info"]["full_name"] = (
        jazzhr_applicant["first_name"] + " " + jazzhr_applicant["last_name"]
    )
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
        ],
    )
    hrflow_profile["tags"] = []
    for tag in list_of_tags:
        if jazzhr_applicant[tag] is not None:
            hrflow_profile["tags"].append(dict(name=tag, value=jazzhr_applicant[tag]))
    hrflow_profile["skills"] = []
    hrflow_profile["experiences"] = []
    hrflow_profile["educations"] = []
    hrflow_profile["languages"] = []
    hrflow_profile["certifications"] = []
    hrflow_profile["interests"] = []
    hrflow_profile["resume"]["raw"] = jazzhr_applicant["base64_resume"]
    return hrflow_profile


def format_jazzhr_archive(jazzhr_entity: t.Dict) -> t.Dict:
    return {"reference": jazzhr_entity["id"]}


def format_hrflow_profile(
    hrflow_profile: t.Dict,
) -> t.Dict:
    jazzhr_applicant = dict(
        first_name=hrflow_profile["info"]["first_name"],
        last_name=hrflow_profile["info"]["last_name"],
        email=hrflow_profile["info"]["email"],
        phone=hrflow_profile["info"]["phone"],
        address=hrflow_profile["info"]["location"]["text"],
        city=hrflow_profile["info"]["location"].get("fields", {}).get("city"),
        state=hrflow_profile["info"]["location"].get("fields", {}).get("state"),
        postal=hrflow_profile["info"]["location"].get("fields", {}).get("postcode"),
        source=hrflow_profile["source"]["name"],
        linkedin=next(
            (
                url["url"]
                for url in hrflow_profile["info"]["urls"]
                if url["type"] == "linkedin"
            ),
            None,
        ),
        twitter=next(
            (
                url["url"]
                for url in hrflow_profile["info"]["urls"]
                if url["type"] == "twitter"
            ),
            None,
        ),
        resumetext=hrflow_profile["text"],
    )
    resume_link = next(
        (
            attachment["public_url"]
            for attachment in hrflow_profile["attachments"]
            if attachment["type"] == "resume"
        ),
        None,
    )
    if resume_link:
        base64_resume = base64.b64encode(requests.get(resume_link).content)
        jazzhr_applicant["base64-resume"] = base64_resume
    return jazzhr_applicant


DESCRIPTION = (
    "JazzHR is a powerful, user-friendly, applicant tracking system "
    "that is purpose-built to help businesses exceed their recruiting goals."
)

JazzHR = Connector(
    name="JazzHR",
    type=ConnectorType.ATS,
    subtype="jazzhr",
    description=DESCRIPTION,
    url="https://www.jazzhr.com",
    warehouse=JazzhrWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_jazzhr_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_jazzhr_job),
        Flow(Mode.archive, Entity.job, Direction.inbound, format=format_jazzhr_archive),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_jazzhr_applicant,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_jazzhr_applicant,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_jazzhr_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
    ),
)
