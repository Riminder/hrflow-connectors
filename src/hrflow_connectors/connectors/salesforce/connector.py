import base64
import json
import typing as t

import requests

from hrflow_connectors.connectors.salesforce.schemas import SalesforceHrFlowProfile
from hrflow_connectors.connectors.salesforce.warehouse import SalesforceProfileWarehouse
from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
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


def get_skills(data: t.Dict) -> str:
    skills = ""
    if data.get("skills") is not None:
        for i in range(len(data["skills"]) - 1):
            skills += data["skills"][i]["name"] + ", "
        skills += data["skills"][-1]["name"]
    return skills


def get_education(education_list: t.List[t.Dict]) -> t.List[t.Dict]:
    educations = []
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
        educations.append(education)
    return educations


def get_experience(experience_list: t.List[t.Dict]) -> t.List[t.Dict]:
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


def get_attachments(attachment_list: t.List[t.Dict]) -> t.List[t.Dict]:
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


def format_profile(data: SalesforceHrFlowProfile) -> t.Dict:
    info = dict(
        full_name=f"{data.Last_Name__c} {data.First_Name__c}",
        first_name=data.First_Name__c,
        last_name=data.Last_Name__c,
        email=data.Email__c,
        phone=data.Phone__c,
        date_birth=data.Date_Birth__c,
        location=dict(
            text=data.Location_Text__c, 
            lat=data.Location_Lat__c,
            lng=data.Location_Lng__c,
            fields=json.loads(data.Location_Fields__c)
        ),
        gender=data.Gender__c,
    )
    experiences = [
        dict(
            title=experience.Title__c,
            location=dict(
                text=experience.Location_Text__c, 
                lat=experience.Location_Lat__c,
                lng=experience.Location_Lng__c,
                fields=json.loads(experience.Location_Fields__c)
            ),
            company=experience.Company__c,
            date_start=experience.Date_Begin__c,
            date_end=experience.Date_End__c,
            description=experience.Description__c,
            skills=json.loads(experience.Skills__c),
            tasks=json.loads(experience.Tasks__c),
            certifications=json.loads(experience.Certifications__c),
        )
        for experience in data.HrFlow_Profile_Experiences__r.records
    ]
    educations = [
        dict(
            title=education.Title__c,
            location=dict(
                text=education.Location_Text__c, 
                lat=education.Location_Lat__c,
                lng=education.Location_Lng__c,
                fields=json.loads(education.Location_Fields__c)
            ),
            school=education.School__c,
            date_start=education.Date_Begin__c,
            date_end=education.Date_End__c,
            description=education.Description__c,
            skills=json.loads(education.Skills__c),
            tasks=json.loads(education.Tasks__c),
            certifications=json.loads(education.Certifications__c),
            courses=json.loads(education.Courses__c),
        )
        for education in data.HrFlow_Profile_Educations__r.records
    ]
    return dict(
        key=data["Hash_Id__c"],
        reference=data["Reference__c"],
        archived_at=data.Archive__c,
        updated_at=data.Date_Edition__c,
        created_at=data.Timestamp__c,
        info=info,
        text_language=data.Text_Language__c,
        text=data.Text__c,
        educations_duration=data.Experiences_Duration__c,
        experiences=experiences,
        educations=educations,
        # FIXME ME add attachments
        attachments=None,
        skills=json.loads(data.Skills__c),
        languages=json.loads(data.Languages__c),
        certifications=json.loads(data.Certifications__c),
        courses=json.loads(data.Courses__c),
        tasks=json.loads(data.Tasks__c),
        interests=json.loads(data.Interests__c),
        labels=json.loads(data.Labels__c),
        tags=json.loads(data.Tags__c),
        metadatas=json.loads(data.Metadatas__c),
    )


DESCRIPTION = (
    "Salesforce is the customer company. We make cloud-based software "
    "designed to help businesses connect to their customers in a whole "
    "new way, so they can find more prospects, close more deals, and wow"
    " customers with amazing service."
)

Salesforce = Connector(
    name="Salesforce",
    description=DESCRIPTION,
    url="https://www.salesforce.com",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from Salesforce HrFlow Custom Objects and writes them to Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_profile
            ),
            origin=SalesforceProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
