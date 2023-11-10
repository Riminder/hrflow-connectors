import re

from hrflow_connectors.connectors.freshteam.warehouse import FreshTeamJobWarehouse
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def html_to_text(html):
    if html is None:
        return None
    text = re.sub(r"<[^>]*>", "", html)
    entity_to_char = {
        "&amp;": "&",
        "&lt;": "<",
        "&gt;": ">",
        "&quot;": '"',
        "&apos;": "'",
    }
    for entity, char in entity_to_char.items():
        text = re.sub(entity, char, text)
    text = re.sub(r"\s+", " ", text).strip()

    return text


def format_tags(job):
    tags = []
    tags_list = [
        "deleted",
        "status",
        "applicant_access_type",
        "remote",
        "show_pursue_as_career",
        "closing_date",
        "experience",
        "type",
    ]
    for tag in tags_list:
        if tag in job:
            tags.append(job[tag])
    if job["branch"]:
        tags.append(job["branch"]["name"])
    if job["department"]:
        tags.append(job["department"]["name"])
    return tags


def format_freshteam_job_to_hrflow(job):
    hrflow_job = {}
    branch = job["branch"]
    location_text = f"{branch['city']}, {branch['state']}, {branch['country_code']}"

    hrflow_job["id"] = job["id"]
    hrflow_job["title"] = job["title"]
    hrflow_job["summary"] = html_to_text(job["description"])
    hrflow_job["created_at"] = job["created_at"]
    hrflow_job["updated_at"] = job["updated_at"]
    hrflow_job["location"] = dict(text=location_text, lat=None, lng=None)
    hrflow_job["ranges_float"] = [
        dict(
            name="salary",
            value_min=job["salary"]["min"],
            value_max=job["salary"]["max"],
            unit=job["salary"]["currency"],
        )
    ]
    hrflow_job["skills"] = []
    hrflow_job["languages"] = []
    hrflow_job["tags"] = format_tags(job)

    return hrflow_job


DESCRIPTION = (
    "Modernize your HR. Manage your hiring, onboarding,"
    "time-off, employee data, and HR workflows in one place."
)

FreshTeam = Connector(
    name="Freshteam",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.freshworks.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from Freshteam  and writes them to an Hrflow.ai board"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_freshteam_job_to_hrflow
            ),
            origin=FreshTeamJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
