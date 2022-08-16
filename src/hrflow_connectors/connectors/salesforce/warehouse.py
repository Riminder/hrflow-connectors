import enum
import json
import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import DataType, Warehouse, WarehouseWriteAction

URI = "https://hrflowai3-dev-ed.my.salesforce.com/services/data/v55.0/sobjects"
ACCEPT = "application/json"
PROFILE_FIELDS = [
    "experience",
    "education",
    "skill",
    "language",
    "certification",
    "course",
    "interest",
    "tag",
    "metadata",
]
GRANT_TYPE = "password"


class ProfileFields(Enum):
    """Class representing a profile field
    get_plural method used to get plural form of a field
    """

    experience = "experience"
    education = "education"
    skill = "skill"
    language = "language"
    certification = "certification"
    course = "course"
    interest = "interest"
    tag = "tag"
    metadata = "metadata"

    def get_plural(self):
        return "{}s".format(self.value)

    def get_salesforce_field(self):
        print("{}__c".format(self.value.capitalize()))
        return "{}__c".format(self.value.capitalize())


class WriteProfilesParameters(BaseModel):
    accept: str = Field(ACCEPT, const=True)
    content_type: str = Field(ACCEPT, const=True)
    grant_type: str = Field(GRANT_TYPE, const=True)
    client_id: str = Field(
        ..., description="Client ID given in Salesforce connected app", repr=False
    )
    client_secret: str = Field(
        ..., description="Client secret given in Salesforce connected app", repr=False
    )
    username: str = Field(..., description="Salesforce account username", repr=False)
    password: str = Field(
        ...,
        description="Salesforce account password concatenated with API security token",
        repr=False,
    )


def get_access_token(
    adapter: LoggerAdapter, parameters: WriteProfilesParameters
) -> str:
    url = "https://login.salesforce.com/services/oauth2/token"
    data = dict(
        grant_type=parameters.grant_type,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
        username=parameters.username,
        password=parameters.password,
    )
    response = requests.post(url, data=data)
    if response.status_code != 200:
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to get access_token using status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
    return json.loads(response.content.decode())["access_token"]


def write_related_object(
    profile: t.Dict, profile_id: str, headers: t.Dict, obj: Enum
) -> str:
    PROFILE_FIELDS = ProfileFields
    print(obj)
    objects_list = profile.get(PROFILE_FIELDS.get_plural(obj))
    if objects_list is not None:
        for instance in objects_list:
            name = (
                instance.get("name")
                if instance.get("name") is not None
                else instance.get("title")
            )
            record = dict(Name=name, Profile__c=profile_id)
            fields = list(filter(lambda x: x != "Name", instance.keys()))
            for field in fields:
                if field == "name":
                    record["Name"] = instance[field]
                elif isinstance(instance[field], str):
                    record["{}__c".format(field)] = instance[field]
            requests.post(
                "{}/{}".format(URI, PROFILE_FIELDS.get_salesforce_field(obj)),
                headers=headers,
                json=record,
            )


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles to Salesforce API".format(len(profiles)))
    failed_profiles = []
    access_token = get_access_token(adapter, parameters)
    for profile in profiles:
        info = dict(
            Name=profile["info"]["full_name"],
            first_name__c=profile["info"]["first_name"],
            last_name__c=profile["info"]["last_name"],
            email__c=profile["info"].get("email"),
            phone__c=profile["info"].get("phone"),
        )
        headers = {
            "Accept": parameters.accept,
            "Content-Type": parameters.content_type,
            "Authorization": "Bearer {}".format(access_token),
        }
        response = requests.post(URI + "/Profile_Info__c", headers=headers, json=info)
        info_id = json.loads(response.content.decode())["id"]
        record = dict(
            Name=profile["info"]["full_name"],
            key__c=profile["key"],
            reference__c=profile["reference"],
            info__c=info_id,
            text_language__c=profile["text_language"],
            text__c=profile["text"],
            archived_at__c=profile.get("archived_at"),
            updated_at__c=profile.get("updated_at"),
            created_at__c=profile.get("created_at"),
            experiences_duration__c=profile.get("experiences_duration"),
            educations_duration__c=profile.get("educations_duration"),
        )
        response = requests.post(
            URI + "/Profile__c",
            headers=headers,
            json=record,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile {} to Salesforce API"
                " status_code={} response={}".format(
                    record["key__c"],
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        profile_id = json.loads(response.content.decode())["id"]
        for field in ProfileFields:
            write_related_object(profile, profile_id, headers, field)
    return failed_profiles


SalesforceProfileWarehouse = Warehouse(
    name="Salesforce Profiles",
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_profiles,
    ),
    data_type=DataType.profile,
)
