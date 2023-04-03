import json
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)

from .schemas import BreezyJobModel


class CompanyId(ParametersModel):
    company_id: t.Optional[str] = Field(
        None,
        description=(
            "[⚠️ Requiered if company_name is not specified], ID of company to pull"
            " jobs from in Breezy HR database associated with the authenticated user"
        ),
        field_type=FieldType.Other,
    )


class CompanyName(ParametersModel):
    company_name: t.Optional[str] = Field(
        None,
        description=(
            "[⚠️ Requiered if company_id is not specified], the company associated with"
            " the authenticated user"
        ),
        field_type=FieldType.Other,
    )


class BreezyhrReadParameters(ParametersModel):
    email: str = Field(..., description="email", field_type=FieldType.Other)

    password: str = Field(..., description="password", field_type=FieldType.Auth)

    company_id: t.Optional[str] = Field(
        None,
        description=(
            "ID of company to pull jobs from in Breezy HR database associated with the"
            " authenticated user"
        ),
        field_type=FieldType.Other,
    )
    company_name: t.Optional[str] = Field(
        None,
        description=(
            "[⚠️ Requiered if company_id is not specified], the company associated with"
            " the authenticated user"
        ),
        field_type=FieldType.Other,
    )


class BreezyHRWriteParameters(ParametersModel):
    email: str = Field(..., description="email", field_type=FieldType.Other)

    password: str = Field(..., description="password", field_type=FieldType.Auth)

    company_id: t.Optional[str] = Field(
        None,
        description=(
            "ID of company to pull jobs from in Breezy HR database associated with the"
            " authenticated user \n [⚠️ Requiered if company_name is not specified]"
        ),
        field_type=FieldType.Other,
    )
    company_name: t.Optional[str] = Field(
        None,
        description=(
            "the company associated with the authenticated user \n [⚠️ Requiered if"
            " company_id is not specified]"
        ),
        field_type=FieldType.Other,
    )

    position_id: str = Field(
        ...,
        description="Id of the position to create a new candidate for",
        field_type=FieldType.Other,
    )
    origin: t.Optional[str] = Field(
        "sourced",
        description=(
            "will indicate in Breezy if the candidate should be marked as sourced or"
            " applied"
        ),
        field_type=FieldType.Other,
    )


def get_access_token(
    adapter: LoggerAdapter,
    parameters: BreezyhrReadParameters,
):
    url = "https://api.breezy.hr/v3/signin"

    payload = json.dumps(
        {"email": f"{parameters.email}", "password": f"{parameters.password}"}
    )
    headers = {"Content-Type": "application/json"}

    response = requests.post(url, headers=headers, data=payload)

    if not response.ok:
        adapter.error(f"Fail to get token, reason: {response.text}")
        raise f"Fail to get token, reason: {response.text}"

    return response.json().get("access_token")


def revoke_access_token(access_token):
    url = "https://api.breezy.hr/v3/signout"

    payload = {}
    headers = {"Content-Type": "application/json", "Authorization": f"{access_token}"}

    requests.get(url, headers=headers, data=payload)


def get_compagnie_id(
    adapter: LoggerAdapter, parameters: BreezyhrReadParameters, access_token: str
):
    if parameters.company_id is not None:
        return parameters.company_id

    url = "https://api.breezy.hr/v3/companies"

    payload = {}
    headers = {"Content-Type": "application/json", "Authorization": f"{access_token}"}

    response = requests.get(url, headers=headers, data=payload)

    if not response.ok:
        adapter.error(f"Fail to get compagnie id, reason: {response.text}")
        raise f"Fail to get compagnie id, reason: {response.text}"

    company_list = response.json()
    for company in company_list:
        if company["name"] == parameters.company_name:
            return company["_id"]

    adapter.error(
        "Fail to get compagnie id, reason: compagny_name does not match with an id"
    )
    raise "Fail to get compagnie id, reason: compagny_name does not match with an id"


def read(
    adapter: LoggerAdapter,
    parameters: BreezyhrReadParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterator[BreezyJobModel]:
    access_token = get_access_token(adapter, parameters)
    compagnie_id = get_compagnie_id(adapter, parameters, access_token)

    url = f"https://api.breezy.hr/v3/company/{compagnie_id}/positions?state=published"

    headers = {"Content-Type": "application/json", "Authorization": f"{access_token}"}

    response = requests.get(url, headers=headers)
    if not response.ok:
        adapter.error(f"Fail to read, reason: {response.text}")
        raise adapter.error(f"Fail to read, reason: {response.text}")

    for job in response.json():
        yield job

    revoke_access_token(access_token)


def send_profile(
    adapter: LoggerAdapter,
    parameters: BreezyHRWriteParameters,
    profiles: t.Iterable[t.Dict],
    access_token: str,
    company_id: str,
    candidate_id: str = "",
):
    base_url = (
        f"https://api.breezy.hr/v3/company/{company_id}/"
        f"position/{parameters.position_id}/candidate"
    )

    payload = json.dumps(profiles)

    headers = {"Content-Type": "application/json", "Authorization": f"{access_token}"}

    if candidate_id == "":
        url = f"{base_url}s/"

        # If the candidate doesn't already exist we "POST" his profile
        response = requests.post(url, headers=headers, data=payload)

    else:
        # In case the candidate exists,
        #  we retrieve his id to update his profile with a "PUT" request

        url = f"{base_url}/{candidate_id}"

        url = (
            f"https://api.breezy.hr/v3/company/{company_id}/"
            f"position/{parameters.position_id}/candidate/{candidate_id}"
        )

        adapter.info(f"Updating id = {candidate_id} profile")
        response = requests.put(url, headers=headers, data=payload)
    return response


def write(
    adapter: LoggerAdapter,
    parameters: BreezyHRWriteParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    access_token = get_access_token(adapter, parameters)
    compagnie_id = get_compagnie_id(adapter, parameters, access_token)

    for profile in profiles:
        profile.update({"origin": parameters.origin})

        mail = profile.get("email_address")

        url = (
            f"https://api.breezy.hr/v3/company/{compagnie_id}/"
            f"candidates/search?email_address={mail}"
        )

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"{access_token}",
        }

        response = requests.get(url, headers=headers)
        if not response.ok:
            adapter.error(f"Couldn't get candidate {profile}")
            failed_profiles.append(profile)
            continue

        candidate_list = response.json()
        candidate_id = ""

        # In case the candidate exists
        # we retrieve his id to update his profile with a "PUT" request
        if candidate_list != []:
            candidate_id = candidate_list[0].get("_id")
            adapter.info(f"Candidate Already exists with the id {candidate_id}")

        response = send_profile(
            adapter, parameters, profile, access_token, compagnie_id, candidate_id
        )
        if not response.ok:
            adapter.error(f"Fail to post user : {profile}")
            failed_profiles.append(profile)

    revoke_access_token(access_token)
    return failed_profiles


BreezyHRJobWarehouse = Warehouse(
    name="BreezyHRJobWarehouse",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=BreezyhrReadParameters,
        function=read,
    ),
)

BreezyHRProfileWarehouse = Warehouse(
    name="BreezyHRWarehouse",
    data_type=DataType.profile,
    write=WarehouseReadAction(
        parameters=BreezyHRWriteParameters,
        function=write,
    ),
)
