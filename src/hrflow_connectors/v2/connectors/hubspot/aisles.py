import typing as t
from logging import LoggerAdapter
from typing import List

import requests
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.hubspot.schemas import ContactObject
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

BASE_URL = "https://api.hubapi.com/crm/v3"
CONTACTS_ENDPOINT = "{}/objects/contacts".format(BASE_URL)


class AuthParameters(Struct):
    access_token: Annotated[
        str,
        Meta(
            description=(
                "The token used to authenticate any API calls made for to your HubSpot"
                " account."
            ),
        ),
    ]


class ReadProfilesParameters(Struct):
    properties: Annotated[
        str,
        Meta(
            description=(
                "A comma separated list of the properties to be returned in the"
                " response. If any of the specified properties are not present on the"
                " requested object(s), they will be ignored."
            ),
        ),
    ] = (
        "firstname,lastname,date_of_birth,email,phone"
        ",company,address,zip,city,state,country"
    )
    propertiesWithHistory: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "A comma separated list of the properties to be returned along with"
                " their history of previous values. If any of the specified properties"
                " are not present on the requested object(s), they will be ignored."
                " Usage of this parameter will reduce the maximum number of objects"
                " that can be read by a single request."
            ),
        ),
    ] = None
    associations: Annotated[
        t.Optional[List[str]],
        Meta(
            description=(
                "A comma separated list of object types to retrieve associated IDs for."
                " If any of the specified associations do not exist, they will be"
                " ignored."
            ),
        ),
    ] = None
    archived: Annotated[
        t.Optional[bool],
        Meta(
            description="Whether to return only results that have been archived.",
        ),
    ] = False


class Stage(Struct):
    label: str
    displayOrder: int
    metadata: str


class Pipeline(Struct):
    id: int
    label: str
    displayOrder: int
    stages: t.List[Stage]


class WriteProfilesParameters(Struct):
    dealID: t.Optional[int] = None


class UpdateProfilesParameters(Struct):
    objectWriteTraceId: t.Optional[str] = None


class ArchiveProfilesParameters(Struct):
    pass


def generic_read(mode: Mode):
    def read(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadProfilesParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        headers = {"Authorization": f"Bearer {auth_parameters.access_token}"}
        params = asdict(parameters)

        def fetch_contacts(params):
            response = requests.get(
                CONTACTS_ENDPOINT,
                headers=headers,
                params=params,
            )
            if response.status_code // 100 != 2:
                adapter.error(
                    f"Failed to pull contacts list from Hubspot. params={params} "
                    f"status_code={response.status_code} response={response.text}"
                )
                raise Exception("Failed to pull contacts list from Hubspot")
            return response.json()

        def should_yield_contact(contact):
            if (
                mode == Mode.create
                and contact["createdAt"][:16] != contact["updatedAt"][:16]
            ):
                return False
            if (
                mode == Mode.update
                and contact["createdAt"][:16] == contact["updatedAt"][:16]
            ):
                return False
            return True

        response_data = fetch_contacts(params)
        contacts = response_data["results"]

        for contact in contacts:
            if should_yield_contact(contact):
                yield contact

        while "paging" in response_data and "next" in response_data["paging"]:
            params["after"] = response_data["paging"]["next"]["after"]
            response_data = fetch_contacts(params)
            contacts = response_data["results"]
            for contact in contacts:
                if should_yield_contact(contact):
                    yield contact

    return read


def write(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        response = requests.post(
            CONTACTS_ENDPOINT,
            headers={
                "Authorization": "Bearer {}".format(auth_parameters.access_token),
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
                    "Authorization": "Bearer {}".format(auth_parameters.access_token),
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


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        contactId = profile.pop("contactId")
        response = requests.patch(
            "{}/{}".format(CONTACTS_ENDPOINT, contactId),
            headers={
                "Authorization": "Bearer {}".format(auth_parameters.access_token),
            },
            json=profile,
            params=asdict(parameters),
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update profile to Hubspot status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        else:
            adapter.info("Successfully updated contact with ID={}".format(contactId))

    return failed_profiles


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        contactId = profile["contactId"]
        response = requests.delete(
            "{}/{}".format(CONTACTS_ENDPOINT, contactId),
            headers={
                "Authorization": "Bearer {}".format(auth_parameters.access_token),
            },
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to archive profile to Hubspot status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        else:
            adapter.info("Successfully archived contact with ID={}".format(contactId))
    return failed_profiles


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=ContactObject,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=generic_read(Mode.create),
            update=generic_read(Mode.update),
            archive=generic_read(Mode.archive),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=UpdateProfilesParameters,
            archive=ArchiveProfilesParameters,
        ),
        function=merge(create=write, update=update, archive=archive),
    ),
)
