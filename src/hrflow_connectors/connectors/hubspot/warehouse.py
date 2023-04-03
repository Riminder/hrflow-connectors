import typing as t
from enum import Enum
from logging import LoggerAdapter
from typing import List

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.hubspot.schemas import ContactObject
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

BASE_URL = "https://api.hubapi.com/crm/v3"


class Properties(str, Enum):  # more properrties can be added later to enrich
    COMPANY = "company"
    EMAIL = "email"
    FIRSTNAME = "firstname"
    LASTNAME = "lastname"
    PHONE = "phone"
    WEBSITE = "website"


class ReadProfilesParameters(ParametersModel):
    access_token: str = Field(
        ...,
        description=(
            "The token used to authenticate any API calls made for to your HubSpot"
            " account."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    limit: int = Field(
        None,
        description="The maximum number of results to display per page.",
        field_type=FieldType.QueryParam,
    )
    after: str = Field(
        None,
        description=(
            "The paging cursor token of the last successfully read resource will be"
            " returned as the `paging.next.after` JSON property of a paged response"
            " containing more results."
        ),
        field_type=FieldType.QueryParam,
    )
    properties: List[Properties] = Field(
        None,
        description=(
            "A comma separated list of the properties to be returned in the response."
            " If any of the specified properties are not present on the requested"
            " object(s), they will be ignored."
        ),
        field_type=FieldType.QueryParam,
    )
    propertiesWithHistory: List[Properties] = Field(
        None,
        description=(
            "A comma separated list of the properties to be returned along with their"
            " history of previous values. If any of the specified properties are not"
            " present on the requested object(s), they will be ignored. Usage of this"
            " parameter will reduce the maximum number of objects that can be read by a"
            " single request."
        ),
        field_type=FieldType.QueryParam,
    )
    associations: List[str] = Field(
        None,
        description=(
            "A comma separated list of object types to retrieve associated IDs for. If"
            " any of the specified associations do not exist, they will be ignored."
        ),
        field_type=FieldType.QueryParam,
    )
    archived: bool = Field(
        default=False,
        description="Whether to return only results that have been archived.",
        field_type=FieldType.QueryParam,
    )


class Stage(BaseModel):
    label: str
    displayOrder: int
    metadata: str


class Pipeline(BaseModel):
    id: int
    label: str
    displayOrder: int
    stages: t.List[Stage]


class WriteProfilesParameters(ParametersModel):
    access_token: str = Field(
        ...,
        description=(
            "The token used to authenticate any API calls made for to your HubSpot"
            " account."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    dealID: t.Optional[int] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    ticketID: t.Optional[int] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    pipeline: t.Optional[Pipeline] = Field(
        None,
        field_type=FieldType.QueryParam,
    )


# TODO: Improve how the contact's profile info is collected individually,
# take into account paggination
def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    url = "{}/objects/contacts".format(BASE_URL)
    params = parameters.dict()
    del params["access_token"]

    response = requests.get(
        url,
        headers={"Authorization": "Bearer {}".format(parameters.access_token)},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull contacts list from Hubspot params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull contacts list from Hubspot")
    contacts = response.json()["results"]
    for contact in contacts:
        yield contact


# TODO: custom pipelines using the endpoint
# and add the contact to it using associations endpoint
def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Adding {} profiles to Hubspot ".format(len(profiles)))
    failed_profiles = []
    contacts_endpoint = "{}/objects/contacts".format(BASE_URL)
    for profile in profiles:
        response = requests.post(
            contacts_endpoint,
            headers={
                "Authorization": "Bearer {}".format(parameters.access_token),
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add profile to Hubspot status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        else:
            contactId = response.json()["id"]
            adapter.info("Successfully added contact with ID={}".format(contactId))
            dealId = parameters.dealID
            if dealId is not None:
                deals_endpoint = (
                    "{}/objects/deals/{}/associations/contacts/{}/3".format(
                        BASE_URL, dealId, contactId
                    )
                )
                headers = {
                    "Authorization": "Bearer {}".format(parameters.access_token),
                }
                deal_response = requests.request(
                    "PUT", deals_endpoint, headers=headers, data={}
                )
                if deal_response.status_code // 100 != 2:
                    adapter.error(
                        "Failed to add profile to Hubspot status_code={} response={}"
                        .format(
                            deal_response.status_code,
                            deal_response.text,
                        )
                    )
    return failed_profiles


HubspotContactWarehouse = Warehouse(
    name="Hubspot Contacts",
    data_schema=ContactObject,
    data_type=DataType.profile,
    read=WarehouseReadAction(parameters=ReadProfilesParameters, function=read),
    write=WarehouseWriteAction(parameters=WriteProfilesParameters, function=write),
)
