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
from hrflow_connectors.core.warehouse import WarehouseWriteAction


class CrossTalentParameters(ParametersModel):
    client_id: str = Field(..., description="Client id", field_type=FieldType.Auth)
    client_secret: str = Field(
        ..., description="Client Secret", field_type=FieldType.Auth
    )

    username: str = Field(..., description="Username", field_type=FieldType.Other)
    password: str = Field(..., description="Password", field_type=FieldType.Auth)

    env: str = Field(..., description="env", field_type=FieldType.Other)
    subdomain: str = Field(
        ...,
        description=(
            "Subdomain Crosstalent just before `salesforce.com`. For example"
            " subdomain=`my_subdomain.my` in"
            " `http://my_subdomain.my.salesforce.com/ABC`"
        ),
        field_type=FieldType.Other,
    )


def get_token(adapter: LoggerAdapter, parameters: CrossTalentParameters) -> str:
    url = f"https://{parameters.env}.salesforce.com/services/oauth2/token"

    payload = {
        "client_id": parameters.client_id,
        "client_secret": parameters.client_secret,
        "username": parameters.username,
        "password": parameters.password,
        "grant_type": "password",
    }

    response = requests.post(url, data=payload)

    if not response.ok:
        adapter.error(f"Fail to get Token {response.content}")
        raise RuntimeError(f"Fail to get token, reason : {response.content}")
    return response.json()["access_token"]


def read(
    adapter: LoggerAdapter,
    parameters: CrossTalentParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    url = (
        f"https://{parameters.subdomain}.salesforce.com/"
        "services/apexrest/crta/HrFlowGetJobOffers/"
    )

    token = get_token(adapter=adapter, parameters=parameters)
    headers = {"Authorization": f"OAuth {token}"}

    response = requests.get(url, headers=headers)

    if not response.ok:
        adapter.error(f"Fail to read jobs, reason : {response.content}")
        raise RuntimeError(f"Fail to read jobs, reason : {response.content}")
    return response.json()


def write(
    adapter: LoggerAdapter,
    parameters: CrossTalentParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    url = (
        f"https://{parameters.subdomain}.salesforce.com/"
        "services/apexrest/crta/HrFlowCreateProfile"
    )

    token = get_token(parameters=parameters)
    headers = {
        "Authorization": f"OAuth {token}",
        "Content-Type": "application/json",
    }
    for profile in profiles:
        try:
            response = requests.post(url, headers=headers, json=profile)
            if not response.ok:
                raise Exception(f"Error with this {profile}")
            adapter.info(
                "Profile sent to Crosstalent and got the following response"
                f" {response.status_code}"
            )
        except Exception:
            failed_profiles.append(profile)
            adapter.error(f"Request failed for {profile}")
    return failed_profiles


CrosstalentJobWarehouse = Warehouse(
    name="CrossTalentJobWarehouse",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=CrossTalentParameters,
        function=read,
    ),
)


CrosstalentProfileWarehouse = Warehouse(
    name="CrossTalentProfileWareHouse",
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=CrossTalentParameters,
        function=write,
    ),
)
