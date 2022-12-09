import base64
import typing as t

import requests

from hrflow_connectors.connectors.flatchr.warehouse import FlatchrProfileWarehouse
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def get_candidate_attachments(hrflow_profile: t.Dict) -> t.Dict:
    attachments_list = hrflow_profile.get("attachments")
    # We try to find 'resume' (which is 'original' after traitments).
    # If we don't find it, we search 'original'.
    fall_backs = ["resume", "original"]

    for file_name in fall_backs:
        for attachment in attachments_list:
            if attachment["file_name"] == file_name:
                # Get the base64's resume from its AWS url.
                url = attachment["public_url"]
                response = requests.get(url)
                b64 = base64.b64encode(response.content)
                return b64.decode()
    return None


def get_education_list(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_education_list = hrflow_profile["educations"]
    flatchr_education_list = []

    for education in hrflow_education_list:
        location = education.get("location")
        flatchr_education = {
            "institution": {
                "communication": {
                    "address": [
                        {"formattedAddress": location.get("text") if location else None}
                    ]
                },
                "name": education.get("school"),
            },
            "educationLevelCodes": [{"name": education.get("title")}],
            "educationDegrees": [
                {
                    "name": education.get("title"),
                    "date": education.get("date_start"),
                    "specializations": [],
                }
            ],
            "end": education.get("date_end"),
            "descriptions": [education.get("description")],
        }
        flatchr_education_list.append(flatchr_education)
    return flatchr_education_list


def get_experiences_list(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_experiences_list = hrflow_profile["experiences"]
    flatchr_experiences_list = []

    for experience in hrflow_experiences_list:
        location = experience.get("location")
        flatchr_education = {
            "title": experience.get("title"),
            "positionHistories": [
                {
                    "organization": {
                        "communication": {
                            "address": [
                                {
                                    "countryCode": None,
                                    "city": None,
                                    "postalCode": None,
                                    "geoLocation": {
                                        "latitude": location.get("lat"),
                                        "longitude": location.get("lng"),
                                    },
                                    "formattedAddress": location.get("text")
                                    if location
                                    else None,
                                }
                            ]
                        },
                        "name": experience.get("company"),
                    },
                    "jobCategories": [],
                    "jobLevels": [],
                    "start": experience.get("date_start"),
                    "current": None,
                }
            ],
            "start": experience.get("date_start"),
            "current": None,
            "descriptions": [experience.get("description")],
        }
        flatchr_experiences_list.append(flatchr_education)
    return flatchr_experiences_list


def get_name() -> t.Dict:
    name = {
        "formattedName": "Undefined",
        "given": "Undefined",
        "family": "Undefined",
    }
    return name


def get_phone(hrflow_profile: t.Dict) -> t.Dict:
    info = hrflow_profile.get("info")
    phone = [{"dialNumber": info.get("phone") if info else "XXXXX", "useCode": None}]
    return phone


def get_email(hrflow_profile: t.Dict) -> t.Dict:
    info = hrflow_profile.get("info")

    email = [{"address": info.get("email") if info else None}]
    return email


def get_position(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_experiences_list = hrflow_profile["experiences"]
    position = []

    for experience in hrflow_experiences_list:
        if experience.get("title") is not None and experience.get("title") != "":
            position.append(experience.get("title"))

    return position


def get_employment_positions(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_experiences_list = hrflow_profile["experiences"]
    employment_positions = []

    for experience in hrflow_experiences_list:
        if experience.get("company") is not None and experience.get("company") != "":
            employment_positions.append(experience.get("company"))

    return employment_positions


def format_profile(hrflow_profile: t.Dict) -> t.Dict:

    # CREATE PROFILE
    data = hrflow_profile

    info = data.get("info")
    email = info.get("email") if info else None
    if email is None:
        reference = data.get("reference")
        raise Exception(f"No email for hrflow_profile {reference} but one is mandatory")

    create_profile_body = {
        "vacancy": None,
        "firstname": info.get("first_name")
        if info.get("first_name") is not None
        else "N/A",
        "lastname": info.get("last_name")
        if info.get("last_name") is not None
        else "N/A",
        "type": "document",
        "email": email,
        "resume": {
            "fileName": "resume.pdf",
            "contentType": "application/octet-stream",
            "data": get_candidate_attachments(data),
        },
    }

    # ENRICH PROFILE

    value = {
        "address": {
            "administrative_area_level_1": data["info"]["location"].get("text"),
            "location_lat": data["info"]["location"].get("lat"),
            "location_lng": data["info"]["location"].get("lng"),
        },
        "education": get_education_list(data),
        "employment": get_experiences_list(data),
        "name": get_name(),
        "phone": get_phone(data),
        "email": get_email(data),
        "position": get_position(data),
        "employment_positions": get_employment_positions(data),
        "experience": int(data.get("experiences_duration")),
    }

    enrich_profile_body = {
        "app_name": "HRMatch",
        "reference": data["info"].get("email"),
        "name": "parsing",
        "type": "applicants",
        "value": value,
    }
    # When the action needs to send several requests to push a profile
    # We group the formats of the different requests in a `profile_body_dict`.
    profile_body_dict = dict(
        create_profile_body=create_profile_body,
        enrich_profile_body=enrich_profile_body,
    )
    return profile_body_dict


DESCRIPTION = (
    "Avec le digital, recruter est devenu beaucoup plus compliqué qu'avant."
    " Flatchr centralise et simplifie vos processus de recrutement pour le rendre aussi"
    " simple que télécharger un film sur Netflix."
)
Flatchr = Connector(
    name="Flatchr",
    description=DESCRIPTION,
    url="https://www.flatchr.io/",
    actions=[
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description="Writes profile from Hrflow.ai Source to Flatchr",
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=FlatchrProfileWarehouse,
        ),
    ],
)
