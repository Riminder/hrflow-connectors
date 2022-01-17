from ...core import action as core
from ...core.auth import OAuth2Session
from ...utils.datetime_converter import from_str_to_datetime
import xml.etree.ElementTree
from pydantic import Field
from ...utils.logger import get_logger

from typing import Union, Dict, Any
import requests
import pprint
import json

TalentDataType = Union[str, xml.etree.ElementTree.Element, Dict[str, Any]]
logger = get_logger()


class PushProfileAction(core.PushProfileAction):

    subdomain: str = Field(
        ...,
        description="Subdomain bullhornstaffing just before `bullhornstaffing.com`. For example "
                    "subdomain=`my_subdomain.my` in "
        "`http://my_subdomain.my.bullhornstaffing.com/ABC`",
    )

    auth: OAuth2Session

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        info = data.get('info')
        def get_location():
   
            if info:
                location = info.get("location")
                location_dict = {
                    "address1": location["fields"].get("text") if location else None,
                    "city": location["fields"].get("city") if location else None,
                    "state": location["fields"].get("country") if location else None,
                    "zip": location["fields"].get("postcode") if location else None
                }
                return location_dict
            return None

        def get_certifications(data):
            certification = ""
            if data.get("certifications"):
                for i in range(len(data["certifications"]) - 1):
                    certification += data["certifications"][i]["value"] + ", "
                certification += data["certifications"][-1]["value"]

        def get_skills(data):
            skills = ""
            if data.get("skills"):
                for i in range(len(data["skills"]) - 1):
                    skills += data["skills"][i]["name"] + ", "
                skills += data["skills"][-1]["name"]
            return skills

        create_profile_body = {
            "id": data.get("reference"),
            "address": get_location(),
            "certifications": get_certifications(data) if data.get("certifications") else None,
            "name": info.get("full_name") if info else None,
            "firstName": info.get("first_name") if info else None,
            "lastName": info.get("last_name") if info else None,
            "email": info.get("email") if info else None,
            "mobile": info.get("phone") if info else None,
            "dateOfBirth": int(from_str_to_datetime(info.get("date_birth")).timestamp()) if info and info.get("date_birth") else None,
            "experience": int(data.get('experiences_duration')),
            "skillSet": get_skills(data) if data.get("skills") else None
        }


        def get_education(education_list):
            educations_json = []
            for education in education_list:
                location = education["location"]
                education = {
                    "id": "0",
                    "candidate": {
                        "id": "None"
                    },
                    "school": education.get("school"),
                    "degree": education.get("title"),
                    "comments": education.get("description"),
                    "city": location.get("text") if location else None,
                    "startDate": int(from_str_to_datetime(education.get("date_start")).timestamp()) if education.get(
                        "date_start") else None,
                    "endDate": int(from_str_to_datetime(education.get("date_end")).timestamp()) if education.get(
                        "date_start") else None
                }
                educations_json.append(education)
            return educations_json

        enrich_profile_education = get_education(data.get("educations"))
        # When the action needs to send several requests to push a profile
        # We group the formats of the different requests in a `profile_body_dict`.
        profile_body_dict = dict(
            create_profile_body=create_profile_body,
            enrich_profile_education=enrich_profile_education,
        )
        return profile_body_dict

    def push(self, data):
        profile_body_dict = next(data)
        create_profile_body = profile_body_dict["create_profile_body"]
        enrich_profile_education = profile_body_dict["enrich_profile_education"]

        # Preparing the request to push the profile
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "PUT"
        push_profile_request.url = f"https://{self.subdomain}.bullhornstaffing.com/rest-services/7zwdd0/entity/Candidate"
        push_profile_request.auth = self.auth
        push_profile_request.json = create_profile_body
        push_profile_request.headers = {"content-type": "application/json"}
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)
        if not response.ok:
            error_message = "Unable to push the data ! Reason : `{}`,`{}`"
            raise RuntimeError(error_message.format(response.status_code,response.content))

        # Get the id of the candidate whom have been just created.
        candidate_id = json.loads(response.text)
        candidate_id = str(candidate_id["changedEntityId"])

        # Preparing the request to enrich education
        for education in enrich_profile_education:

            # Set the Id of the candidate to enrich to the Id of the candidate whom have just been created
            education["candidate"]["id"] = candidate_id
            session = requests.Session()
            push_profile_request = requests.Request()
            push_profile_request.method = "PUT"
            push_profile_request.url = f"https://{self.subdomain}.bullhornstaffing.com/rest-services/7zwdd0/entity/CandidateEducation"
            push_profile_request.auth = self.auth
            push_profile_request.json = education
            push_profile_request.headers = {"content-type": "application/json"}
            prepared_request = push_profile_request.prepare()

            # Send request for enrichment
            response = session.send(prepared_request)
            if not response.ok:
                error_message = "Unable to push the data ! Reason : `{}`,`{}`"
                raise RuntimeError(error_message.format(response.status_code,response.content))
