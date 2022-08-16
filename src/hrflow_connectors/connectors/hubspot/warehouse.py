import json
import typing as t
from enum import Enum
from logging import LoggerAdapter
from typing import List

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.hubspot.schemas import ContactObject
from hrflow_connectors.core import Warehouse, WarehouseWriteAction
from hrflow_connectors.core.warehouse import DataType

baseUrl = "https://api.hubapi.com/crm/v3"


class Properties(str, Enum):  # TODO: more properrties can be added later to enrich
    COMPANY = "company"
    EMAIL = "email"
    FIRSTNAME = "firstname"
    LASTNAME = "lastname"
    PHONE = "phone"
    WEBSITE = "website"


class ReadProfilesParameters(BaseModel):
    access_token: str = Field(
        ...,
        description=(
            "The token used to authenticate any API calls made for to your HubSpot"
            " account."
        ),
        repr=False,
    )
    limit: int = Field(
        None, description="The maximum number of results to display per page."
    )
    after: str = Field(
        None,
        description=(
            "The paging cursor token of the last successfully read resource will be"
            " returned as the `paging.next.after` JSON property of a paged response"
            " containing more results."
        ),
    )
    properties: List[Properties] = Field(
        None,
        description=(
            "A comma separated list of the properties to be returned in the response."
            " If any of the specified properties are not present on the requested"
            " object(s), they will be ignored."
        ),
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
    )
    associations: List[
        str
    ] = Field(  # TODO:learn more about associations and implement this filter better
        None,
        description=(
            "A comma separated list of object types to retrieve associated IDs for. If"
            " any of the specified associations do not exist, they will be ignored."
        ),
    )
    archived: bool = Field(
        default=False,
        description="Whether to return only results that have been archived.",
    )


class WriteProfilesParameters(BaseModel):
    access_token: str = Field(
        ...,
        description=(
            "The token used to authenticate any API calls made for to your HubSpot"
            " account."
        ),
        repr=False,
    )
    # TODO: Pipeline and sequences to be added to the parameters


# TODO: Improve how the contact's profile info is collected individually
def read(
    adapter: LoggerAdapter, parameters: ReadProfilesParameters
) -> t.Iterable[t.Dict]:

    url = f"{baseUrl}/objects/contacts"
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


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Adding {} profiles to Hubspot ".format(len(profiles)))
    failed_profiles = []
    url = f"{baseUrl}/objects/contacts"

    for profile in profiles:
        payload = json.dumps(profile)

        response = requests.post(
            url,
            headers={
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(parameters.access_token),
            },
            data=payload,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add profile to Hubspot status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


HubspotContactWarehouse = Warehouse(
    name="Hubspot Contacts",
    data_schema=ContactObject,
    data_type=DataType.profile,
    # read=WarehouseReadAction(parameters=ReadProfilesParameters, function=read),
    write=WarehouseWriteAction(parameters=WriteProfilesParameters, function=write),
)
