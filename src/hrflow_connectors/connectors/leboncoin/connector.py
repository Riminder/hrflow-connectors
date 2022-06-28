import json
import re
import typing as t
import uuid

from geotext import GeoText

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.leboncoin.warehouse import LeboncoinWarehouse
from hrflow_connectors.core import BaseActionParameters, Connector, ConnectorAction

MORPHEUS_CLIENT_ID = None
SECRETS_JSON_PATH = "src/hrflow_connectors/connectors/leboncoin/secrets.json"
DEFAULT_CITY = "Paris"
DEFAULT_ZIP_CODE = 75001
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
    if location.get("text"):
        location_text = location["text"]
        zip_code = location["fields"].get("postalcode")
        print(zip_code)
        city = location["fields"].get("city")
        country = location["fields"].get("country")
        if zip_code is not None:
            result_match = re.search("[0-9]{4,5}", location_text)
            if result_match:
                zip_code = result_match.group(0)
    else:
        zip_code = DEFAULT_ZIP_CODE
    zip_code = zip_code if zip_code is not None else DEFAULT_ZIP_CODE
    city = city if city is not None else DEFAULT_CITY
    if country is not None:
        return dict(zip_code=zip_code, city=city, country=country)
    return dict(zip_code=zip_code, city=city)


def get_contract_type(tags: t.List[t.Dict]) -> str:
    contract = next((tag for tag in tags if tag["name"] == "contract"), None)
    if contract:
        return CONTRACT_CODES[contract["value"]]
    else:
        raise ValueError("Could not find contract type")


def get_applicant(job: t.Dict) -> t.Dict:
    applicant = dict()
    skills = ", ".join([skill for skill in job.get("skills", [])])
    if skills:
        applicant["skills"] = skills
    return applicant


def get_application(tags: t.List[t.Dict]) -> t.Dict:
    mode = next(filter(lambda x: x["name"] == "mode", tags), None)
    contact = next(filter(lambda x: x["name"] == "contact", tags), None)
    if mode is None or contact is None:
        raise ValueError("Could not extract Application (Mode or Contact missing")
    return dict(mode=mode["value"], contact=contact["value"])


def format_job(job: t.Dict) -> t.Dict:
    job_leboncoin = dict()
    if job.get("reference"):
        job_leboncoin["client_reference"] = job.get("reference")
    job_leboncoin.update(
        dict(
            title=job.get("name"),
            description=(
                job.get("sections")[0].get("description")
                if job.get("sections")[0] is not None
                else ""
            ),
            contract_type=get_contract_type(job.get("tags")),
            location=get_job_location(job.get("location")),
        )
    )
    return job_leboncoin


def format_ad(job: t.Dict) -> t.Dict:
    ad = dict()
    with open(SECRETS_JSON_PATH) as f:
        MORPHEUS_CLIENT_ID = json.load(f)["MORPHEUS_CLIENT_ID"]
    ad["morpheus_client_id"] = MORPHEUS_CLIENT_ID
    ad.update(
        dict(
            partner_unique_reference=str(uuid.uuid1()),
            job=format_job(job),
            application=get_application(job.get("tags")),
        )
    )
    if get_applicant(job):
        ad["applicant"] = get_applicant(job)
    return ad


# Note that we've only implemented the required attributes of the Ad object,
# optional attributes may require additional constraints on HrFlow Job JSONs


DESCRIPTION = "Yet to be written"
Leboncoin = Connector(
    name="Leboncoin",
    description=DESCRIPTION,
    url="https://www.leboncoin.com/",
    actions=[
        ConnectorAction(
            name="push_jobs",
            description=(
                "Retrieves all jobs from an HrFlow JobBoard and sends them"
                " through the Leboncoin API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteAdsParameters", format=format_ad
            ),
            origin=HrFlowJobWarehouse,
            target=LeboncoinWarehouse,
        )
    ],
)
