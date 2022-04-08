from email import header
from logging import LoggerAdapter
import typing as t

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.crosstalent.schemas import CrosstalentJob
from hrflow_connectors.core import warehouse
from hrflow_connectors.core.warehouse import ActionEndpoints, WarehouseReadAction

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
    subdomain: str = (
        Field(
            ...,
            description=(
                "Subdomain Crosstalent just before `salesforce.com`. For example"
                " subdomain=`my_subdomain.my` in"
                " `http://my_subdomain.my.salesforce.com/ABC`"
            ),
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

    # Prepare request
    session = requests.Session()
    pull_jobs_request = requests.Request()
    pull_jobs_request.method = "GET"
    pull_jobs_request.url = "https://{}.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/".format(
        parameters.subdomain
    )
    pull_jobs_request.headers = {
        "Authorization": f"OAuth {access_token}"
    }
    prepared_request = pull_jobs_request.prepare()

    # Send request
    response = session.send(prepared_request)

    if not response.ok:
        raise RuntimeError("Failed to pull jobs reason {}".format(response.content))

    return response.json()    


CrosstalentJobWarehouse = warehouse(
    name="Crosstalent Jobs",
    data_schema=CrosstalentJob,
    read=WarehouseReadAction(
        parameters=PullJobsParameters,
        function=read,
        endpoints=[GET_ALL_JOBS_ENDPOINT],
    ),
)
