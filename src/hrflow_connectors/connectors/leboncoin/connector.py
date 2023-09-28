import json
import re
import typing as t
import uuid

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.leboncoin.warehouse import LeboncoinWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

MORPHEUS_CLIENT_ID = None
SECRETS_JSON_PATH = "src/hrflow_connectors/connectors/leboncoin/secrets.json"
DEFAULT_CITY = "No City"
DEFAULT_ZIP_CODE = "99999"
CONTRACT_CODES = {
    "CDD": "1",
    "CDI": "2",
    "Interim": "3",
    "Indépendant": "4",
    "Stage": "5",
    "Alternance": "5",
    "Apprentissage": "6",
}


def get_job_location(location: t.Dict) -> t.Dict:
    """Extracts the location data from a job location dictionary and formats it

    Args:
        location (t.Dict): HrFlow Job location

    Returns:
        t.Dict: Leboncoin job location
    """
    zip_code = DEFAULT_ZIP_CODE
    city = DEFAULT_CITY
    country = None
    if isinstance(location["fields"], dict):
        zip_code, city, country = (
            location["fields"].get("postalcode"),
            location["fields"].get("city"),
            location["fields"].get("country"),
        )
    if zip_code == DEFAULT_ZIP_CODE and location["text"]:
        result_match = re.search("[0-9]{4,5}", location["text"])
        zip_code = result_match.group(0) if result_match else None
    return (
        dict(zip_code=zip_code, city=city, country=country)
        if country is not None
        else dict(zip_code=zip_code, city=city)
    )


def get_contract_type(tags: t.List[t.Dict]) -> t.Union[None, str]:
    """Extracts the type of contract from the job tags if they contain a contract type tag

    Args:
        tags (t.List[t.Dict]): List of job tags

    Returns:
        t.Union[None,str]: the contract type as a string if it exists, otherwise None
    """
    contract = next((tag for tag in tags if tag["name"] == "contract"), None)
    return CONTRACT_CODES[contract["value"]] if contract is not None else None


def get_applicant(job: t.Dict) -> t.Dict:
    """Extracts the applicant from the Hrflow Job

    Args:
        job (t.Dict): HrFlow Job

    Returns:
        t.Dict: Leboncoin job applicant
    """
    skills = ", ".join([skill["name"] for skill in job.get("skills", [])])
    return dict(skills=skills) if skills else {}


def get_application(tags: t.List[t.Dict]) -> t.Union[t.Dict, None]:
    """Extracts the application from the Hrflow Job tags

    Args:
        tags (t.List[t.Dict]): HrFlow Job tags list

    Returns:
        t.Union[t.Dict,None]: Leboncoin application if it exists, otherwise None
    """
    mode = next(filter(lambda x: x["name"] == "mode", tags), None)
    contact = next(filter(lambda x: x["name"] == "contact", tags), None)
    if mode is None or contact is None:
        return None
    return (
        dict(mode=mode["value"], contact=contact["value"])
        if mode is not None and contact is not None
        else None
    )


def format_job(job: t.Dict) -> t.Dict:
    """formats the Leboncoin job from HrFlow job

    Args:
        job (t.Dict): Hrflow job

    Returns:
        t.Dict: Leboncoin job
    """
    job_leboncoin = dict()  # create Leboncoin job object from HrFlow job
    if job.get("reference"):  # if there is a reference use it
        job_leboncoin["client_reference"] = job.get("reference")
    job_leboncoin.update(
        dict(
            title=job.get("name"),
            description=(
                "This is a default message, submit the job description manually"
            ),
            contract_type=get_contract_type(job["tags"]),
            location=get_job_location(job["location"]),
        )
    )
    return job_leboncoin


def format_ad(job: t.Dict) -> t.Dict:
    """formats an ad from Leboncoin job

    Args:
        job (t.Dict): Hrflow job

    Returns:
        t.Dict: Leboncoin ad
    """
    ad = dict()
    with open(SECRETS_JSON_PATH) as f:
        MORPHEUS_CLIENT_ID = json.load(f)["MORPHEUS_CLIENT_ID"]
    ad["morpheus_client_id"] = MORPHEUS_CLIENT_ID
    ad.update(
        dict(
            partner_unique_reference=str(uuid.uuid1()),
            job=format_job(job),
            application=get_application(job["tags"]),
        )
    )
    if get_applicant(job):
        ad["applicant"] = get_applicant(job)
    return ad


DESCRIPTION = (
    "With leboncoin, find the right deal on the leading site for"
    "classified ads from private individuals and professionals."
)
Leboncoin = Connector(
    name="Leboncoin",
    type=ConnectorType.Classifieds,
    description=DESCRIPTION,
    url="https://www.leboncoin.com/",
    actions=[
        ConnectorAction(
            name=ActionName.push_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs from an HrFlow JobBoard and sends them"
                " through the Leboncoin API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteAdsParameters", format=format_ad
            ),
            origin=HrFlowJobWarehouse,
            target=LeboncoinWarehouse,
            action_type=ActionType.outbound,
        )
    ],
)
