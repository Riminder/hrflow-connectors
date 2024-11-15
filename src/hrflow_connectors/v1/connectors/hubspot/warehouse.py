import typing as t
from logging import LoggerAdapter
from typing import List

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)
from hrflow_connectors.v1.connectors.hubspot.schemas import ContactObject

BASE_URL = "https://api.hubapi.com/crm/v3"
CONTACTS_ENDPOINT = "{}/objects/contacts".format(BASE_URL)


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
    properties: str = Field(
        default=(
            "firstname,lastname,date_of_birth,email,phone"
            ",company,address,zip,city,state,country"
        ),
        description=(
            "A comma separated list of the properties to be returned in the response."
            " If any of the specified properties are not present on the requested"
            " object(s), they will be ignored."
        ),
        field_type=FieldType.QueryParam,
    )
    propertiesWithHistory: str = Field(
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


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    headers = {"Authorization": f"Bearer {parameters.access_token}"}
    params = parameters.dict()
    del params["access_token"]

    response = requests.get(
        CONTACTS_ENDPOINT,
        headers=headers,
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            f"Failed to pull contacts list from Hubspot.  params={params}"
            f" status_code={response.status_code} response={response.text}"
        )
        raise Exception("Failed to pull contacts list from Hubspot")
    contacts = response.json()["results"]
    for contact in contacts:
        yield contact
    while "paging" in response.json() and "next" in response.json()["paging"]:
        next_url = response.json()["paging"]["next"]["link"]
        response = requests.get(
            next_url, headers=headers, params=dict(properties=parameters.properties)
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull contacts list from Hubspot."
                f" status_code={response.status_code} response={response.text}"
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
    for profile in profiles:
        response = requests.post(
            CONTACTS_ENDPOINT,
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
