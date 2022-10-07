import re
import typing as t

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.teamtailor.schema import TeamtailorCandidateAttribute
from hrflow_connectors.connectors.teamtailor.warehouse import (
    TeamtailorJobWarehouse,
    TeamtailorProfileWarehouse,
)
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def remove_html_tags(text: str) -> str:
    """
    Remove all HTML tags in a string
    Args:
        text (str): text to clean
    Returns:
        str: cleaned text (without HTML tags)
    """
    return re.sub("<[^<]+?>", "", text)


# Returns 0 if not a valid float
def str_to_float(value: str) -> float:
    if value.replace(".", "", 1).isdigit():
        return float(value)
    return 0


def create_tag(field_name: str, attribute: t.Dict, job: t.Dict) -> None:
    tag_name = "teamtailor_{}".format(field_name)
    tag_value = attribute.get(field_name)

    if tag_value is not None:
        tag = dict(name=tag_name, value=tag_value)
        job["tags"].append(tag)


def format_job(data: t.Dict) -> HrFlowJob:
    """
    Format a Teamtailor job object into a Hrflow job object
    Args:
        data (TeamtailorJob): Teamtailor job object from the list of jobs pulled
    Returns:
        HrflowJob: a job in the HrFlow job object form
    """
    job = dict()
    job_data = data.get("job").get("data")
    attribute = job_data.get("attributes")
    job["name"] = attribute.get("title")
    job["reference"] = job_data.get("id")
    job["summary"] = attribute.get("pitch")
    job["created_at"] = attribute.get("created-at")
    job["updated_at"] = attribute.get("updated-at")
    job["url"] = job_data.get("links").get("careersite-job-url")

    job_location = data.get("job_location")
    text = job_location.get("text")
    lat = str_to_float(job_location.get("lat"))
    lng = str_to_float(job_location.get("lng"))
    job["location"] = dict(text=text, lat=lat, lng=lng)

    # sections
    description = remove_html_tags(attribute.get("body"))
    job["sections"] = [
        dict(
            name="teamtailor_description",
            title="teamtailor_description",
            description=description,
        )
    ]
    # tags
    job["tags"] = []
    return job


def format_profile(data: t.Dict) -> TeamtailorCandidateAttribute:
    """
    Format a Hrflow profile object into a Teamtailor profile object
    Args:
        data (HrflowProfile): Hrflow Profile to format
    Returns:
        Dict[str, Any]: a Teamtailor formatted profile object
    """
    profile = dict()
    info = data.get("info")
    profile["first-name"] = info.get("first_name")
    profile["last-name"] = info.get("last_name")
    profile["email"] = info.get("email")
    profile["phone"] = info.get("phone")
    pitch = info.get("summary")
    profile["pitch"] = pitch[0:139]
    profile["resume"] = ""
    if len(data.get("attachments")) >= 2:
        resume = data.get("attachments")[1]
        profile["resume"] = resume.get("public_url")
    profile["sourced"] = "sourced"
    profile["tags"] = data.get("tags")
    profile_obj = dict()
    profile_obj["data"] = {"type": "candidates", "attributes": profile}
    return profile_obj


DESCRIPTION = "Teamtailor"

Teamtailor = Connector(
    name="Teamtailor",
    description=DESCRIPTION,
    url="https://www.teamtailor.com/",
    actions=[
        ConnectorAction(
            name="pull_jobs",
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieve all jobs via the ***Teamtailor*** API and send them"
                " to an ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=TeamtailorJobWarehouse,
            target=HrFlowJobWarehouse,
        ),
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from an Hrflow.ai Source to Teamtailor via the API"
                " for a given `job_id`."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=TeamtailorProfileWarehouse,
        ),
    ],
)
