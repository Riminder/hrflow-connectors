import typing as t
from datetime import datetime
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.workable.schemas import WorkableJobModel
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)
from hrflow_connectors.core.warehouse import WarehouseWriteAction


class State(str, Enum):
    draft = "draft"
    published = "published"
    archived = "archived"
    closed = "closed"


class WorkableReadParameters(ParametersModel):
    auth: str = Field(..., description="API KEY", field_type=FieldType.Auth)
    subdomain: str = Field(..., description="Subdomain", field_type=FieldType.Auth)


class WorkableWriteParameters(ParametersModel):
    auth: str = Field(..., description="API KEY", field_type=FieldType.Auth)
    subdomain: str = Field(..., description="Subdomain", field_type=FieldType.Other)
    shortcode: str = Field(..., description="Job shortcode", field_type=FieldType.Other)


class WorkableProfilesReadParameters(ParametersModel):
    auth: str = Field(..., description="API KEY", field_type=FieldType.Auth)
    subdomain: str = Field(..., description="Subdomain", field_type=FieldType.Auth)
    shortcode: t.Optional[str] = Field(
        ..., description="Job shortcode", field_type=FieldType.Other
    )
    state: t.Optional[str] = Field(
        description="The job's stage slug, can be retrieved from the /stages endpoint",
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        description="Specifies the number of candidates to try and retrieve per page",
        field_type=FieldType.QueryParam,
    )
    since_id: t.Optional[str] = Field(
        description=(
            "Returns results with an ID greater than or equal to the specified ID."
        ),
        field_type=FieldType.QueryParam,
    )
    max_id: t.Optional[str] = Field(
        description=(
            "Returns results with an ID less than or equal to the specified ID."
        ),
        field_type=FieldType.QueryParam,
    )
    created_after: t.Optional[datetime] = Field(
        description="Returns results created after the specified timestamp.",
        field_type=FieldType.QueryParam,
    )
    updated_after: t.Optional[datetime] = Field(
        description="Returns results updated after the specified timestamp.",
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: WorkableReadParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterator[WorkableJobModel]:
    url = f"https://{parameters.subdomain}.workable.com/spi/v3/jobs?state=published"

    payload = {}
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {parameters.auth}",
    }

    response = requests.get(url, headers=headers, data=payload)

    if not response.ok:
        adapter.error(f"Fail to read Workable jobs reason : {response.content}")

    for job in response.json().get("jobs"):
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    parameters: WorkableProfilesReadParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterator[t.Dict]:
    url = f"https://{parameters.subdomain}.workable.com/spi/v3/candidates"
    # Set the headers with the API key
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {parameters.auth}",
    }

    # Set the parameters for the API call
    params = dict()
    params["shortcode"] = parameters.shortcode
    params["state"] = parameters.state
    params["limit"] = parameters.limit
    params["since_id"] = parameters.since_id
    params["max_id"] = parameters.max_id
    params["created_after"] = parameters.created_after
    params["updated_after"] = parameters.updated_after

    # Make the GET request
    response = requests.get(url, headers=headers, params=params)

    # Check if the request was successful
    if not response.ok:
        adapter.error(f"Fail to read Workable profiles reason : {response.content}")
    # Collect candidates IDs from the response
    candidates_ids = [candidate["id"] for candidate in response.json()["candidates"]]
    # Iterate over the candidates IDs and make a GET request for each candidate
    for candidate_id in candidates_ids:
        url = (
            f"https://{parameters.subdomain}.workable.com/"
            "spi/v3/candidates/{candidate_id}"
        )
        response = requests.get(url, headers=headers)
        # Check if the request was successful
        if not response.ok:
            adapter.error(f"Fail to read Workable profile reason : {response.content}")
        # Yield the candidate
        yield response.json()["candidate"]


def write(
    adapter: LoggerAdapter,
    parameters: WorkableWriteParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile in profiles:
        url = (
            f"https://{parameters.subdomain}.workable.com/"
            f"spi/v3/jobs/{parameters.shortcode}/candidates"
        )

        payload = profile

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {parameters.auth}",
            "Accept": "application/json",
        }

        response = requests.post(url, headers=headers, data=payload)
        if not response.ok:
            adapter.error(f"Error append with this profile : {profile}")
            failed_profiles.append(profile)

    return failed_profiles


WorkableJobWarehouse = Warehouse(
    name="WorkableJobWarehouse",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=WorkableReadParameters,
        function=read,
    ),
)

WorkableProfileWarehouse = Warehouse(
    name="WorkableProfileWarehouse",
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WorkableWriteParameters,
        function=write,
    ),
    read=WarehouseReadAction(
        parameters=WorkableProfilesReadParameters,
        function=read_profiles,
    ),
)
