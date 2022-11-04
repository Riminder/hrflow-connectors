import base64
import typing as t

import requests

from hrflow_connectors.connectors.bullhorn.utils import date_format
from hrflow_connectors.connectors.bullhorn.warehouse import BullhornProfileWarehouse
from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def to_int(elm: t.Any) -> int:
    if elm is None:
        return 0
    return int(elm)


def get_location(info: t.Dict) -> t.Dict:
    if info is not None:
        location = info.get("location")
        if location is None:
            location = dict()
        fields = location.get("fields", {})
        if fields == []:
            fields = {}
        location_dict = {
            "address1": location.get("text"),
            "city": fields.get("city"),
            "state": fields.get("country"),
            "zip": fields.get("postcode"),
        }
        return location_dict
    return None


def get_skills(data: t.Dict) -> t.Dict:
    skills = ""
    if data.get("skills") is not None:
        for i in range(len(data["skills"]) - 1):
            skills += data["skills"][i]["name"] + ", "
        skills += data["skills"][-1]["name"]
    return skills


def get_education(education_list: t.Dict) -> t.Dict:
    educations_json = []
    for hrflow_education in education_list:
        location = hrflow_education["location"]
        education = {
            "id": "0",
            "candidate": {"id": None},
            "school": hrflow_education.get("school"),
            "degree": hrflow_education.get("title"),
            "comments": hrflow_education.get("description"),
            "city": location.get("text") if location else None,
            "startDate": int(
                date_format.from_str_to_datetime(
                    hrflow_education.get("date_start")
                ).timestamp()
            )
            if hrflow_education.get("date_start")
            else None,
            "endDate": int(
                date_format.from_str_to_datetime(
                    hrflow_education.get("date_end")
                ).timestamp()
            )
            if hrflow_education.get("date_start")
            else None,
        }
        educations_json.append(education)
    return educations_json


def get_experience(experience_list: t.Dict) -> t.Dict:
    experience_json = []
    for hrflow_experience in experience_list:
        experience = {
            "id": "0",
            "candidate": {"id": None},
            "companyName": hrflow_experience.get("company"),
            "title": hrflow_experience.get("title"),
            "comments": hrflow_experience.get("description"),
            "startDate": int(
                date_format.from_str_to_datetime(
                    hrflow_experience.get("date_start")
                ).timestamp()
            )
            if hrflow_experience.get("date_start")
            else None,
            "endDate": int(
                date_format.from_str_to_datetime(
                    hrflow_experience.get("date_end")
                ).timestamp()
            )
            if hrflow_experience.get("date_end")
            else None,
        }
        experience_json.append(experience)
    return experience_json


def get_attachments(attachment_list: t.Dict) -> t.Dict:
    attachments_json = []
    for hrflow_attachment in attachment_list:
        url = hrflow_attachment["public_url"]
        response = requests.get(url)
        b64 = base64.b64encode(response.content)

        attachment = {
            "externalID": "portfolio",
            "fileContent": b64.decode(),
            "fileType": "SAMPLE",
            "name": hrflow_attachment["file_name"],
            "contentType": "text/plain",
            "description": "Resume file for candidate.",
            "type": "cover",
        }
        attachments_json.append(attachment)
    return attachments_json


def format_profile(data: HrFlowProfile) -> t.Dict:
    info = data.get("info")

    dateOfBirth = None
    if info is not None and info.get("date_birth"):
        date_birth_field = info.get("date_birth")
        date_birth_timestamp = date_format.from_str_to_datetime(
            date_birth_field
        ).timestamp()
        dateOfBirth = int(date_birth_timestamp)

    create_profile_body = {
        "id": data.get("reference"),
        "address": get_location(info),
        "certifications": None,
        "name": info.get("full_name") if info else None,
        "firstName": info.get("first_name") if info else None,
        "lastName": info.get("last_name") if info else None,
        "email": info.get("email") if info else None,
        "mobile": info.get("phone") if info else None,
        "dateOfBirth": dateOfBirth,
        "experience": to_int(data.get("experiences_duration")),  # TODO
        "skillSet": get_skills(data) if data.get("skills") else None,
    }

    enrich_profile_education = get_education(data.get("educations"))
    enrich_profile_experience = get_experience(data.get("experiences"))
    enrich_profile_attachment = get_attachments(data.get("attachments"))
    # Four querys are needed to index a Candidate to Bullhorn
    # The querys's body are grouped in a profile_body_dict
    profile_body_dict = dict(
        create_profile_body=create_profile_body,
        enrich_profile_education=enrich_profile_education,
        enrich_profile_experience=enrich_profile_experience,
        enrich_profile_attachment=enrich_profile_attachment,
    )
    return profile_body_dict


DESCRIPTION = "Transform Your Business with Bullhorn Staffing and Recruitment Software"

Bullhorn = Connector(
    name="Bullhorn",
    description=DESCRIPTION,
    url="https://www.bullhorn.com/",
    actions=[
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source to Bullhorn via the API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=BullhornProfileWarehouse,
        ),
    ],
)
