import json
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import Warehouse, WarehouseWriteAction

URI = "https://hrflowai3-dev-ed.my.salesforce.com/services/data/v55.0/sobjects"
ACCEPT = "application/json"


class WriteProfilesParameters(BaseModel):
    accept: str = Field(ACCEPT, const=True)
    content_type: str = Field(ACCEPT, const=True)
    access_token: str = Field(..., description=" ", repr=False)


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles to Salesforce API".format(len(profiles)))
    failed_profiles = []
    for profile in profiles:
        info = dict(Name=profile["info"]["full_name"])
        headers = {
            "Accept": parameters.accept,
            "Content-Type": parameters.content_type,
            "Authorization": "Bearer {}".format(parameters.access_token),
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
    return failed_profiles


class WriteJobsParameters(BaseModel):
    accept: str = Field(ACCEPT, const=True)
    content_type: str = Field(ACCEPT, const=True)
    access_token: str = Field(..., description=" ", repr=False)


def write_jobs(
    adapter: LoggerAdapter,
    parameters: t.Dict,
    jobs: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} jobs to Salesforce API".format(len(jobs)))
    failed_jobs = []
    for job in jobs:
        location = dict(
            Name="Location " + job["key"],
            text__c=job["location"]["text"],
            lat__c=job["location"]["lat"],
            lng__c=job["location"]["lng"],
        )
        headers = {
            "Accept": parameters.accept,
            "Content-Type": parameters.content_type,
            "Authorization": "Bearer {}".format(parameters.access_token),
        }
        response = requests.post(URI + "/Location__c", headers=headers, json=location)
        location_id = json.loads(response.content.decode())["id"]
        record = dict(
            Name=job["name"],
            key__c=job["key"],
            reference__c=job["reference"],
            location__c=location_id,
        )
        response = requests.post(URI + "/Job__c", headers=headers, json=record)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push job to Salesforce API job_key={}"
                " status_code={} response={}".format(
                    record["key__c"],
                    response.status_code,
                    response.text,
                )
            )
            failed_jobs.append(job)
    return failed_jobs


SalesforceProfileWarehouse = Warehouse(
    name="Salesforce Profiles",
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_profiles,
    ),
)

SalesforceJobsWarehouse = Warehouse(
    name="Salesforce Jobs",
    write=WarehouseWriteAction(
        parameters=WriteJobsParameters,
        function=write_jobs,
    ),
)
