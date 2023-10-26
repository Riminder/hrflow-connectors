from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.jobadder.warehouse import (
    JobadderJobWarehouse,
    JobadderProfileWarehouse,
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


def format_job(job: dict) -> dict:
    hrflow_job = dict()
    hrflow_job["title"] = job["jobTitle"]
    hrflow_job["created_at"] = job["createdAt"]
    hrflow_job["updated_at"] = job["updatedAt"]
    hrflow_job["description"] = job["jobDescription"]

    hrflow_job["location"] = dict(
        text=job["location"]["name"] + " " + job["location"]["area"]["name"],
        lat=None,
        lng=None,
    )
    hrflow_job["skills"] = [
        {"name": tag, "type": None, "value": None} for tag in job["skillTags"]["tags"]
    ]
    hrflow_job["ranges_float"] = [
        {
            "name": "salary",
            "value_min": job["salary"]["rateLow"],
            "value_max": job["salary"]["rateHigh"],
            "currency": job["salary"]["currency"],
        }
    ]
    hrflow_job["tags"] = []
    hrflow_job["languages"] = []
    hrflow_job["sections"] = []

    return hrflow_job


def format_profile(profile: dict) -> dict:
    hrflow_profile = dict()
    hrflow_profile["reference"] = profile["candidateId"]
    hrflow_profile["created_at"] = profile["createdAt"]
    hrflow_profile["updated_at"] = profile["updatedAt"]
    hrflow_profile["info"] = dict()
    hrflow_profile["info"]["first_name"] = profile["firstName"]
    hrflow_profile["info"]["last_name"] = profile["lastName"]
    hrflow_profile["info"]["email"] = profile["email"]
    hrflow_profile["info"]["phone"] = profile["phone"]
    hrflow_profile["info"]["date_birth"] = profile["dateOfBirth"]
    hrflow_profile["info"]["location"] = dict(
        text=profile["address"]["street"][0]
        + " "
        + profile["address"]["city"]
        + " "
        + profile["address"]["state"],
        lat=None,
        lng=None,
    )
    hrflow_profile["summary"] = profile["summary"]
    hrflow_profile["skills"] = [
        {"name": tag, "type": None, "value": None} for tag in profile["skillTags"]
    ]
    hrflow_profile["educations"] = format_educations(profile["educations"])
    hrflow_profile["experiences"] = format_experiences(profile["employments"])
    hrflow_profile["languages"] = []
    hrflow_profile["interests"] = []
    hrflow_profile["tags"] = []
    return hrflow_profile


def format_educations(educations):
    profile_educations = []
    for education in educations:
        item = dict(
            school=education["institution"],
            description=education["course"],
            location=dict(text=None, lat=None, lng=None),
            skills=[],
            courses=[],
            certificiations=[],
            tasks=[],
            start_date=education["date"],
            end_date=None,
        )
        profile_educations.append(item)
    return profile_educations


def format_experiences(employment):
    profile_experiences = []
    experiences_history = employment["history"]
    experience_current = employment["current"]
    for experience in experiences_history:
        item = dict(
            company=experience["employer"],
            title=experience["position"],
            date_sart=experience["start"],
            date_end=experience["end"],
            description=experience["description"],
            skills=[],
            courses=[],
            certificiations=[],
            tasks=[],
        )
        profile_experiences.append(item)
    if experience_current:
        item = dict(
            company=experience_current["employer"],
            title=experience_current["position"],
            date_sart=None,
            date_end=None,
            description=None,
            skills=[],
            courses=[],
            certificiations=[],
            tasks=[],
        )
        profile_experiences.append(item)
    return profile_experiences


def format_profile_to_jobadder(profile_hrflow):
    profile_jobadder = dict()
    profile_jobadder["firstName"] = profile_hrflow["info"]["first_name"]
    profile_jobadder["lastName"] = profile_hrflow["info"]["last_name"]
    profile_jobadder["email"] = profile_hrflow["info"]["email"]
    profile_jobadder["phone"] = profile_hrflow["info"]["phone"]
    profile_jobadder["dateOfBirth"] = profile_hrflow["info"]["date_birth"]
    profile_jobadder["address"] = dict(
        street=None, city=None, state=None, postcode=None, country=None
    )
    profile_jobadder["summary"] = profile_hrflow["summary"]
    profile_jobadder["skillTags"] = [
        skill["name"] for skill in profile_hrflow["skills"]
    ]
    profile_jobadder["educations"] = format_educations_to_jobadder(
        profile_hrflow["educations"]
    )
    profile_jobadder["employments"] = dict(
        history=format_experiences_to_jobadder(profile_hrflow["experiences"])
    )
    return profile_jobadder


def format_educations_to_jobadder(educations):
    profile_educations = []
    for education in educations:
        item = dict(
            institution=education["school"],
            course=education["description"],
            date=education["start_date"],
        )
        profile_educations.append(item)
    return profile_educations


def format_experiences_to_jobadder(experiences: dict):
    profile_experiences = []
    for experience in experiences:
        item = dict(
            employer=experience["company"],
            position=experience["title"],
            start=experience["date_start"],
            end=experience["date_end"],
            description=experience["description"],
        )
        profile_experiences.append(item)
    return profile_experiences


DESCRIPTION = (
    "JobAdder is simplifying recruitment for recruitment agencies, "
    "staffing firms and HR and talent acquisition teams who want everything "
    "in one place without the complexity. Use it as a standalone platform or "
    "plug and play with 10+ add-ons, 100+ partners and 200+ job boards to create "
    "a custom recruitment management solution. "
)

Jobadder = Connector(
    name="Jobadder",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://jobadder.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from Jobadder "
                " and writes them to an Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_profile
            ),
            origin=JobadderProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Retrieves profiles from an Hrflow.ai source "
                "and writes them to Jobadder"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile_to_jobadder
            ),
            origin=HrFlowProfileWarehouse,
            target=JobadderProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from Jobadder and writes them to an Hrflow.ai board "
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_job
            ),
            origin=JobadderJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
