from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.zoho.warehouse import (
    ZohoCandidatesWarehouse,
    ZohoJobWarehouse,
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


def format_job_openings(job_openings: dict) -> dict:
    hrflow_job = {}
    location_text = (
        job_openings["City"]
        + ", "
        + job_openings["State"]
        + ", "
        + job_openings["Country"]
    )
    hrflow_job["reference"] = job_openings["JobOpeningID"]
    hrflow_job["title"] = job_openings["PostingTitle"]
    hrflow_job["description"] = job_openings["JobDescription"]
    hrflow_job["location"] = {"text": location_text, "lat": None, "lng": None}
    hrflow_job["requirement"] = job_openings["JobSummary"]
    hrflow_job["created_at"] = job_openings["DateOpened"]
    hrflow_job["sections"] = []
    tags_list = [
        "AccountManager",
        "AssignedRecruiter",
        "ClientName",
        "ContactName",
        "Industry",
        "JobType",
        "State",
        "WorkExperience",
        "Country",
        "ModifiedBy",
        "Salary",
    ]
    hrflow_job["tags"] = []
    for tag in tags_list:
        if job_openings[tag]:
            hrflow_job["tags"].append(job_openings[tag])
    return hrflow_job


def format_profile(candidate: dict) -> dict:
    hrflow_profile = {}
    location_text = (
        candidate["zipCode"]
        + ", "
        + candidate["city"]
        + ", "
        + candidate["state"]
        + ", "
        + candidate["country"]
    )
    hrflow_profile["reference"] = candidate["candidateID"]
    hrflow_profile["info"]["first_name"] = candidate["firstName"]
    hrflow_profile["info"]["last_name"] = candidate["lastName"]
    hrflow_profile["info"]["email"] = candidate["email"]
    hrflow_profile["info"]["phone"] = candidate["phone"]
    hrflow_profile["info"]["location"] = {
        "text": location_text,
        "lat": None,
        "lng": None,
    }
    hrflow_profile["info"]["summary"] = candidate["skillSet"]
    hrflow_profile["experiences"] = []
    hrflow_profile["educations"] = []
    hrflow_profile["skills"] = []
    hrflow_profile["languages"] = []
    hrflow_profile["interests"] = []
    hrflow_profile["tags"] = []
    tags_list = [
        "candidateOwner",
        "source",
        "highestQualificationHeld",
        "website",
        "candidateStatus",
        "emailOptOut",
        "currentEmployer",
        "expectedSalary",
        "currentSalary",
        "additionalInfo",
        "twitter",
        "modifiedBy",
        "createdBy",
        "experienceInYears",
        "currentJobTitle",
        "mobile",
        "fax",
        "street",
        "resume",
        "formattedResume",
    ]
    for tag in tags_list:
        if candidate[tag]:
            hrflow_profile["tags"].append(candidate[tag])

    return hrflow_profile


def format_candidate(profile: dict) -> dict:
    zoho_candidate = {}
    zoho_candidate["firstName"] = profile["info"]["first_name"]
    zoho_candidate["lastName"] = profile["info"]["last_name"]
    zoho_candidate["email"] = profile["info"]["email"]
    zoho_candidate["phone"] = profile["info"]["phone"]
    zoho_candidate["skillSet"] = profile["info"]["summary"]
    zoho_candidate["candidateID"] = profile["reference"]

    return zoho_candidate


DESCRIPTION = (
    "Zoho Corporation is a prominent multinational SaaS (Software as a Service)"
    "company offering a wide array of cloud-based software applications and services "
    "for businesses. Known for its user-friendly and customizable tools, Zoho "
    "empowers users to manage work, collaborate, and automate tasks efficiently."
    "With a global reach, competitive pricing, and a strong emphasis on data privacy "
    "and security, Zoho is a popular choice for businesses seeking productivity "
    "and management solutions. "
)

Zoho = Connector(
    name="Zoho",
    type=ConnectorType.CRM,
    description=DESCRIPTION,
    url="https://www.zoho.com",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from Zoho  and writes them to an Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_profile
            ),
            origin=ZohoCandidatesWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Pushs specific Profile from HrFlow and writes it to a Zoho Candidate"
            ),
            parameters=BaseActionParameters.with_defaults(
                "PushProfileActionParameters", format=format_candidate
            ),
            origin=HrFlowProfileWarehouse,
            target=ZohoCandidatesWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from zoho  and writes them to an Hrflow.ai board"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_job_openings
            ),
            origin=ZohoJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
