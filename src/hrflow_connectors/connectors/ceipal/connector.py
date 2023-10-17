from hrflow_connectors.connectors.ceipal.warehouse import (
    CeipalJobWarehouse,
    CeipalProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
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


def format_job(ceipal_job: dict) -> dict:
    hrflow_job = {}
    text = " ,".join(
        [
            ceipal_job["city"] if ceipal_job["city"] else "",
            ceipal_job["state"] if ceipal_job["state"] else "",
            ceipal_job["country"] if ceipal_job["country"] else "",
        ]
    )
    hrflow_job["reference"] = ceipal_job["id"]
    hrflow_job["created_at"] = ceipal_job["created"]
    hrflow_job["updated_at"] = ceipal_job["modified"]
    hrflow_job["name"] = ceipal_job["position_title"]
    hrflow_job["sections"] = [
        {
            "name": "description",
            "title": "description",
            "description": ceipal_job["requisition_description"],
        }
    ]
    hrflow_job["location"] = {"text": text, "lat": None, "lng": None}
    hrflow_job["skills"] = [
        {"name": skill, "value": None} for skill in ceipal_job["skills"].split(",")
    ]
    hrflow_job["ranges_float"] = job_ranges(ceipal_job["pay_rates"])
    hrflow_job["tags"] = job_tags(ceipal_job)
    return hrflow_job


def job_tags(ceipal_job: dict) -> dict:
    hrflow_tags = []
    tags = [
        "business_unit_id",
        "assigned_recruiter",
        "posted_by",
        "duration",
        "experience",
        "min_experience",
        "job_start_date",
        "job_end_date",
        "modified",
        "work_authorization",
        "number_of_positions",
        "closing_date",
        "remote_opportunities",
        "public_job_desc",
        "public_job_title",
        "employment_type",
        "primary_recruiter",
        "department",
        "currency",
        "job_status",
        "industry",
        "tax_terms",
        "apply_job",
        "apply_job_without_registration",
        "contact_person",
        "posted",
    ]
    hrflow_tags = [
        {"name": tag, "value": ceipal_job[tag]} for tag in tags if ceipal_job[tag]
    ]
    return hrflow_tags


def job_ranges(pay_rates: dict) -> dict:
    hrflow_ranges = []
    for range in pay_rates:
        min_value, max_value = range["pay_rate"].split("-")
        hrflow_ranges.append(
            {
                "name": "pay_rate",
                "min_value": min_value,
                "max_value": max_value,
                "unit": range["pay_rate_currency"],
            }
        )
    return hrflow_ranges


def format_profile(ceipal_profile: dict) -> dict:
    hrflow_profile = {}
    text = " ,".join(
        [
            ceipal_profile["city"] if ceipal_profile["city"] else "",
            ceipal_profile["state"] if ceipal_profile["state"] else "",
            ceipal_profile["country"] if ceipal_profile["country"] else "",
            ceipal_profile["address"] if ceipal_profile["address"] else "",
        ]
    )

    hrflow_profile["reference"] = ceipal_profile["id"]
    hrflow_profile["created_at"] = ceipal_profile["created_at"]
    hrflow_profile["info"]["first_name"] = ceipal_profile["firstname"]
    hrflow_profile["info"]["last_name"] = ceipal_profile["lastname"]
    hrflow_profile["info"]["full_name"] = (
        ceipal_profile["firstname"] + " " + ceipal_profile["lastname"]
    )
    hrflow_profile["info"]["email"] = ceipal_profile["email"]
    hrflow_profile["info"]["phone"] = ceipal_profile["mobile_number"]
    hrflow_profile["info"]["location"] = {"text": text, "lat": None, "lng": None}
    hrflow_profile["info"]["urls"] = [
        {"type": "resume", "url": ceipal_profile["resume_path"]}
    ]
    hrflow_profile["skills"] = [
        {"name": skill, "value": None} for skill in ceipal_profile["skills"].split(",")
    ]
    hrflow_profile["experiences"] = []
    hrflow_profile["educations"] = []
    hrflow_profile["languages"] = []
    hrflow_profile["tags"] = profile_tags(ceipal_profile)
    return hrflow_profile


def profile_tags(ceipal_profile: dict) -> dict:
    hrflow_tags = []
    tags = [
        "applicant_id",
        "middlename",
        "consultant_name",
        "email_address_1",
        "other_phone",
        "applicant_status",
        "job_title",
        "home_phone_number",
        "work_phone_number",
        "created_by",
    ]
    hrflow_tags = [
        {"name": tag, "value": ceipal_profile[tag]}
        for tag in tags
        if ceipal_profile[tag]
    ]
    return hrflow_tags


def format_applicant(hrflow_profile: HrFlowProfile) -> dict:
    pass


DESCRIPTION = (
    "Ceipal ATS Software is the best recruiting and talent acquisition software"
    "offering intelligent, integrated, scalable staffing software solutions."
)

Ceipal = Connector(
    name="Ceipal",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.ceipal.com",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from Ceipal and writes them to an Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_profile
            ),
            origin=CeipalProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description="Pushs specific Profile from HrFlow and writes it to Ceipal",
            parameters=BaseActionParameters.with_defaults(
                "PushProfileActionParameters", format=format_applicant
            ),
            origin=HrFlowProfileWarehouse,
            target=CeipalProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from Ceipal and writes them to an Hrflow.ai board"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_job
            ),
            origin=CeipalJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
