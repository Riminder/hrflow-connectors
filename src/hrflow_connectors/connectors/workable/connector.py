import json
import re
import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.workable.warehouse import (
    WorkableJobWarehouse,
    WorkableProfileWarehouse,
)
from hrflow_connectors.core.connector import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

from ..hrflow.schemas import HrFlowJob, HrFlowProfile
from .schemas import WorkableCandidate, WorkableJobModel


def remove_html_tags(text: str) -> str:
    """
    Remove all HTML tags in a string
    Args:
        text (str): text to clean
    Returns:
        str: cleaned text (without HTML tags)
    """
    return re.sub("<[^<]+?>", "", text)


def format_jobs(workable_job: WorkableJobModel) -> HrFlowJob:
    """
    Format a job into the hrflow job object format
    Args:
        data (WorkableJobModel): a job object pulled from workable subdomain
    Returns:
        HrflowJob: a job into the hrflow job object format
    """
    hrflow_job = dict()
    # name and reference
    hrflow_job["name"] = workable_job.get("title")
    hrflow_job["reference"] = workable_job.get("shortcode")
    # url
    hrflow_job["url"] = workable_job.get("url")
    # location
    location = workable_job.get("location")
    location_str = location.get("location_str")
    text = None
    if isinstance(location_str, str):
        text = location_str
    geojson = dict()

    def get_geojson(field_name: str):
        if location.get(field_name) is not None:
            geojson["field_name"] = location.get(field_name)

    get_geojson("country")
    get_geojson("country_code")
    get_geojson("region_code")
    get_geojson("region")
    get_geojson("city")
    get_geojson("zip_code")
    get_geojson("telecommuting")
    hrflow_job["location"] = dict(text=text, geojson=geojson)
    # sections
    hrflow_job["sections"] = []

    def create_section(field_name: str):
        title_name = f"workable_{field_name}"
        field_value = workable_job.get(field_name)
        if isinstance(field_value, str):
            description = remove_html_tags(field_value)
            section = dict(name=title_name, title=title_name, description=description)
            hrflow_job["sections"].append(section)

    create_section("description")
    create_section("requirements")
    create_section("benefits")
    # creation_date
    hrflow_job["created_at"] = workable_job.get("created_at")
    # tags
    hrflow_job["tags"] = []

    def create_tag(field_name):
        name = f"workable_{field_name}"
        field_value = workable_job.get("field_name")
        if field_value is not None:
            tag = dict(name=name, value=field_value)
            hrflow_job["tags"].append(tag)

    create_tag("employment_type")
    create_tag("full_title")
    create_tag("id")
    create_tag("code")
    create_tag("state")
    create_tag("department")
    create_tag("application_url")
    create_tag("shortlink")
    create_tag("employment_type")

    return hrflow_job


def format_profile(
    hrflow_profile: HrFlowProfile,
) -> WorkableCandidate:
    """
    Format a HrflowProfile object into a WorkableCandidate object
    Args:
        data (HrflowProfile): HrflowProfile object
    Returns:
        WorkableCandidate: WorkableCandidate object
    """
    candidate_profile = dict()

    info = hrflow_profile.get("info")

    candidate_profile["name"] = info.get("full_name")
    candidate_profile["summary"] = info.get("summary")
    candidate_profile["email"] = info.get("email")
    candidate_profile["phone"] = info.get("phone")
    location = info.get("location")

    if isinstance(location.get("text"), str):
        candidate_profile["address"] = location.get("text")
    attachments = hrflow_profile.get("attachments")

    if isinstance(attachments, list):
        for attachment in attachments:
            if isinstance(attachment, dict):
                if attachment["type"] == "resume":
                    candidate_profile["resume_url"] = attachment["public_url"]

    workable_profile = dict(sourced=True, candidate=candidate_profile)
    return json.dumps(workable_profile)


def format_experiences(
    workable_experiences: t.List,
) -> t.List:
    """
    Format a Workable experience object into a Hrflow experience object
    Args:
        data (Workable experience): Workable experience object
    Returns:
        Hrflow experience: Hrflow experience object
    """
    experiences = []
    for experience in workable_experiences:
        hrflow_experience = dict()
        hrflow_experience["title"] = experience.get("title")
        hrflow_experience["company"] = experience.get("company")
        hrflow_experience["description"] = experience.get("description")
        hrflow_experience["date_start"] = experience.get("start_date")
        hrflow_experience["date_end"] = experience.get("end_date")
        experiences.append(hrflow_experience)
    return experiences


def format_educations(
    workable_educations: t.List,
) -> t.List:
    """
    Format a Workable education object into a Hrflow education object
    Args:
        data (Workable education): Workable education object
    Returns:
        Hrflow education: Hrflow education object
    """
    educations = []
    for education in workable_educations:
        hrflow_education = dict()
        hrflow_education["school"] = education.get("school")
        hrflow_education["title"] = education.get("degree")
        hrflow_education["date_start"] = education.get("start_date")
        hrflow_education["date_end"] = education.get("end_date")
        educations.append(hrflow_education)
    return educations


def format_candidate(
    workabal_candidate: t.Dict,
) -> HrFlowProfile:
    """
    Format a WorkableCandidate object into a HrflowProfile object
    Args:
        data (WorkableCandidate): WorkableCandidate object
    Returns:
        HrFlowProfile: HrflowProfile object
    """
    hrflow_profile = dict()
    hrflow_profile["reference"] = workabal_candidate.get("id")
    hrflow_profile["info"] = dict()
    hrflow_profile["info"]["full_name"] = workabal_candidate.get("name")
    hrflow_profile["info"]["first_name"] = workabal_candidate.get("firstname")
    hrflow_profile["info"]["last_name"] = workabal_candidate.get("lastname")
    hrflow_profile["info"]["summary"] = workabal_candidate.get("summary")
    hrflow_profile["info"]["email"] = workabal_candidate.get("email")
    hrflow_profile["info"]["phone"] = workabal_candidate.get("phone")
    hrflow_profile["info"]["location"] = dict(lat=None, lng=None)
    hrflow_profile["info"]["location"]["text"] = workabal_candidate.get("address")
    hrflow_profile["info"]["urls"] = [
        dict(type=social_profile["type"], url=social_profile["url"])
        for social_profile in workabal_candidate.get("social_profiles")
    ]
    hrflow_profile["experiences"] = format_experiences(
        workabal_candidate.get("experience_entries")
    )
    hrflow_profile["educations"] = format_educations(
        workabal_candidate.get("education_entries")
    )
    hrflow_profile["tags"] = []
    hrflow_profile["tags"].append(
        dict(
            name="workable_job_shortcode",
            value=workabal_candidate.get("job").get("shortcode"),
        )
    )
    hrflow_profile["tags"].append(
        dict(
            name="workable_job_title", value=workabal_candidate.get("job").get("title")
        )
    )
    hrflow_profile["tags"].append(
        dict(name="workable_job_stage", value=workabal_candidate.get("stage"))
    )
    hrflow_profile["attachments"] = []
    attachment = dict()
    attachment["type"] = "resume"
    attachment["public_url"] = workabal_candidate.get("resume_url")
    hrflow_profile["attachments"].append(attachment)
    return hrflow_profile


Workable = Connector(
    name="Workable",
    type=ConnectorType.HCM,
    description=(
        "More than an applicant tracking system, "
        "Workable's talent acquisition software helps teams find candidates, "
        "evaluate applicants and make the right hire, faster."
    ),
    url="https://www.workable.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***Workable*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullJobsActionParameters", format=format_jobs
            ),
            origin=WorkableJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from ***Hrflow.ai Source*** to ***Workable*** via the"
                " API for the given `shortcode`."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=WorkableProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all profiles via the ***Workable*** API and send them"
                " to a ***Hrflow.ai Source***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullProfilesActionParameters", format=format_candidate
            ),
            origin=WorkableProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
