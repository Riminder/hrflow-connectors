from ....core.auth import AuthorizationAuth
from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....utils.hrflow import generate_workflow_response

from pydantic import Field
from typing import Dict, Any, Iterator
import base64
import requests


class PushProfile(ProfileDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: AuthorizationAuth
    subdomain: str = Field(
        ...,
        description="Subdomain flatchr just before `flatchr.io`. For example subdomain=`my_subdomain.my` in "
        "`http://my_subdomain.my.flatchr.io/ABC`",
    )
    vacancy: str = Field(
        ...,
        description="The pool in which candidates will be placed. Findable in the URL",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        self.headers["Accept"] = "*/*"

    @property
    def base_url(self):
        return "https://{}.flatchr.io/vacancy/candidate/json".format(self.subdomain)

    @property
    def http_method(self):
        return "POST"

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to flatchr failed : `{}`".format(response.content)
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        def get_candidate_attachments(hrflow_profile):
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

        info = data.get("info")
        email = info.get("email") if info else None
        if email == None:
            raise Exception(
                f"No email for hrflow_profile {data.get('reference')} but one is mandatory"
            )

        profile = {
            "vacancy": self.vacancy,
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
        return profile


class EnrichProfile(ProfileDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: AuthorizationAuth
    subdomain: str = Field(
        ...,
        description="Subdomain flatchr just before `flatchr.io`. For example subdomain=`my_subdomain.my` in "
        "`http://my_subdomain.my.flatchr.io/ABC`",
    )
    vacancy: str = Field(
        ...,
        description="The pool in which candidates will be placed. Findable in the URL",
    )
    compagny: str = Field(
        ...,
        description="The id of the compagny",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "https://{0}.flatchr.io/company/{1}/search/candidate".format(
            self.subdomain, self.compagny
        )

    @property
    def http_method(self):
        return "POST"

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to flatchr failed : `{}`".format(response.content)
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        def get_education_list(hr_flow_profile):
            hrflow_education_list = hr_flow_profile["educations"]
            flatchr_education_list = []

            for education in hrflow_education_list:
                location = education.get("location")
                flatchr_education = {
                    "institution": {
                        "communication": {
                            "address": [
                                {
                                    "formattedAddress": location.get("text")
                                    if location
                                    else None
                                }
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

        def get_experiences_list(hr_flow_profile):
            hrflow_experiences_list = hr_flow_profile["experiences"]
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

        def get_name():
            name = {"formattedName": "XXX XXXX", "given": "XXXX", "family": "XXXX"}
            return name

        def get_phone(hr_flow_profile):
            info = hr_flow_profile.get("info")
            phone = [
                {"dialNumber": info.get("phone") if info else None, "useCode": None}
            ]
            return phone

        def get_email(hr_flow_profile):
            info = hr_flow_profile.get("info")

            email = [{"address": info.get("email") if info else None}]
            return email

        def get_position(hr_flow_profile):
            hrflow_experiences_list = hr_flow_profile["experiences"]
            position = []

            for experience in hrflow_experiences_list:
                if experience.get("title") != None and experience.get("title") != "":
                    position.append(experience.get("title"))

            return position

        def get_employment_positions(hr_flow_profile):
            hrflow_experiences_list = hr_flow_profile["experiences"]
            employment_positions = []

            for experience in hrflow_experiences_list:
                if (
                    experience.get("company") != None
                    and experience.get("company") != ""
                ):
                    employment_positions.append(experience.get("company"))

            return employment_positions

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

        profile = {
            "app_name": "HRMatch",
            "reference": data["info"].get("email"),
            "name": "parsing",
            "type": "applicants",
            "value": value,
        }

        return profile