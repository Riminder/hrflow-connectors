import json
import typing as t
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


def get_access_token(parameters: WriteProfilesParameters):
    url = "https://login.salesforce.com/services/oauth2/token"
    data = dict(
        grant_type=parameters.grant_type,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
        username=parameters.username,
        password=parameters.password,
    )
    response = requests.post(url, data=data)
    return json.loads(response.content.decode())["access_token"]


def write_related_object(
    profile: t.Dict, profile_id: str, headers: t.Dict, obj_name: str
) -> str:
    n = 0
    objects_list = profile.get("{}s".format(obj_name))
    if objects_list is not None:
        n = 0
        for instance in objects_list:
            name = instance.get("name", obj_name + str(n))
            n += 1 if name == obj_name + str(n) else n
            record = dict(Name=name, Profile__c=profile_id)
            fields = list(filter(lambda x: x != "Name", instance.keys()))
            for field in fields:
                if field == "name":
                    record["Name"] = instance[field]
                elif isinstance(instance[field], str):
                    record["{}__c".format(field)] = instance[field]
            requests.post(
                URI + "/{}__c".format(obj_name),
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
    access_token = get_access_token(parameters)
    for profile in profiles:
        info = dict(Name=profile["info"]["full_name"])
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
        for field in PROFILE_FIELDS:
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
