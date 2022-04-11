import typing as t
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.crosstalent.schemas import CrosstalentJob
from hrflow_connectors.core.warehouse import (
    ActionEndpoints,
    Warehouse,
    WarehouseReadAction,
)

CROSSTALENT_JOBS_ENDPOINT = "https://vulcain-eng--preprod.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/"

GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description=(
        "Endpoint to search jobs by traditional params (offset, limit...)"
        " and get the list of all jobs with their ids, the request method"
        " is `GET`"
    ),
    url="https://vulcain-eng--preprod.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/",
)


class PullJobsParameters(BaseModel):
    env: str = Field(..., description="Environnement: test, stagin, production")
    subdomain: str = Field(
        ...,
        description=(
            "Subdomain Crosstalent just before `salesforce.com`. For example"
            " subdomain=`my_subdomain.my` in"
            " `http://my_subdomain.my.salesforce.com/ABC`"
        ),
    )

    client_secret: str = Field(..., description="Crosstalent secret key")
    client_id: str = Field(..., description="Crosstalent id")
    username: str = Field(..., description="Username")
    password: str = Field(..., description="Password")


def get_access_token(adapter: LoggerAdapter, parameters: PullJobsParameters) -> str:
    access_token_url = f"https://{parameters.env}.salesforce.com/services/oauth2/token"
    payload = dict()
    payload["grant_type"] = "password"
    payload["client_id"] = parameters.client_id
    payload["client_secret"] = parameters.client_secret
    payload["username"] = parameters.username
    payload["password"] = parameters.password

    response = requests.post(access_token_url, data=payload)

    if not response.ok:
        adapter.error("Could not get access token")
        raise RuntimeError("OAuth2 failed ! Reason : `{}`".format(response.content))
    return response.json()["access_token"]


def read(adapter: LoggerAdapter, parameters: PullJobsParameters) -> t.Iterable[t.Dict]:

    access_token = get_access_token(adapter=adapter, parameters=parameters)

    response = requests.get(
        "https://{}.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/".format(
            parameters.subdomain
        ),
        headers={"Authorization": f"OAuth {access_token}"},
    )

    if not response.ok:
        raise RuntimeError("Failed to pull jobs reason {}".format(response.content))

    return response.json()


CrosstalentJobWarehouse = Warehouse(
    name="Crosstalent Jobs",
    data_schema=CrosstalentJob,
    read=WarehouseReadAction(
        parameters=PullJobsParameters,
        function=read,
        endpoints=[GET_ALL_JOBS_ENDPOINT],
    ),
)
