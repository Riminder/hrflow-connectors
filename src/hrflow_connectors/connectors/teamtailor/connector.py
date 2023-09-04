import re
import typing as t

from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.teamtailor.schema import TeamtailorJob
from hrflow_connectors.connectors.teamtailor.warehouse import (
    TeamtailorJobWarehouse,
    TeamtailorProfileWarehouse,
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


def format_job(data: TeamtailorJob) -> t.Dict:
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
    t = lambda name, value: dict(name=name, value=value)
    job["tags"] = [
        t("start-date", attribute.get("start-date")),
        t("end-date", attribute.get("end-date")),
        t("status", attribute.get("status")),
        t("employment-type", attribute.get("employment-type")),
        t("employment-level", attribute.get("employment-level")),
        t("remote-status", attribute.get("remote-status")),
        t("salary-time-unit", attribute.get("salary-time-unit")),
        t("min-salary", attribute.get("min-salary")),
        t("max-salary", attribute.get("max-salary")),
        t("currency", attribute.get("currency")),
        t("internal", attribute.get("internal")),
    ]

    return job


def format_profile(data: HrFlowProfile) -> t.Dict:
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


DESCRIPTION = (
    "The new way to attract, nurture and hire top talent. Grow faster by focusing on"
    " what matters the most — your candidates"
)

Teamtailor = Connector(
    name="Teamtailor",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.teamtailor.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
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
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from an Hrflow.ai Source to Teamtailor via the API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=TeamtailorProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
