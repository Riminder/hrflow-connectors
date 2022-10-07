import json
import re

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.workable.warehouse import (
    WorkableJobWarehouse,
    WorkableProfileWarehouse,
)
from hrflow_connectors.core.connector import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
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


def format_jobs(workable_jobs: WorkableJobModel) -> HrFlowJob:
    """
    Format a job into the hrflow job object format
    Args:
        data (WorkableJobModel): a job object pulled from workable subdomain
    Returns:
        HrflowJob: a job into the hrflow job object format
    """
    job = dict()
    data = workable_jobs
    # name and reference
    job["name"] = data.get("title")
    job["reference"] = data.get("shortcode")
    # url
    job["url"] = data.get("url")
    # location
    location = data.get("location")
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
    job["location"] = dict(text=text, geojson=geojson)
    # sections
    job["sections"] = []

    def create_section(field_name: str):
        title_name = "workable_{}".format(field_name)
        field_value = data.get(field_name)
        if isinstance(field_value, str):
            description = remove_html_tags(field_value)
            section = dict(name=title_name, title=title_name, description=description)
            job["sections"].append(section)

    create_section("description")
    create_section("requirements")
    create_section("benefits")
    # creation_date
    job["created_at"] = data.get("created_at")
    # tags
    job["tags"] = []

    def create_tag(field_name):
        name = "workable_{}".format(field_name)
        field_value = data.get("field_name")
        if field_value is not None:
            tag = dict(name=name, value=field_value)
            job["tags"].append(tag)

    create_tag("employment_type")
    create_tag("full_title")
    create_tag("id")
    create_tag("code")
    create_tag("state")
    create_tag("department")
    create_tag("application_url")
    create_tag("shortlink")
    create_tag("employment_type")

    return job


def format_profile(
    profile: HrFlowProfile,
) -> WorkableCandidate:  # HrFlow profiles -> Workable Profiles
    """
    Format a HrflowProfile object into a WorkableCandidate object
    Args:
        data (HrflowProfile): HrflowProfile object
    Returns:
        WorkableCandidate: WorkableCandidate object
    """
    data = profile
    info = data.get("info")
    profile = dict()
    profile["name"] = info.get("full_name")
    profile["summary"] = info.get("summary")
    profile["email"] = info.get("email")
    profile["phone"] = info.get("phone")
    location = info.get("location")

    if isinstance(location.get("text"), str):
        profile["address"] = location.get("text")
    attachments = data.get("attachments")

    if isinstance(attachments, list):
        for attachment in attachments:
            if isinstance(attachment, dict):
                if attachment["type"] == "resume":
                    profile["resume_url"] = attachment["public_url"]

    candidate_profile = dict(sourced=True, candidate=profile)
    return json.dumps(candidate_profile)


Workable = Connector(
    name="Workable",
    description=(
        "More than an applicant tracking system, "
        "Workable's talent acquisition software helps teams find candidates, "
        "evaluate applicants and make the right hire, faster."
    ),
    url="https://www.workable.com/",
    actions=[
        ConnectorAction(
            name="pull_jobs",
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
        ),
        ConnectorAction(
            name="push_profile",
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
        ),
    ],
)
