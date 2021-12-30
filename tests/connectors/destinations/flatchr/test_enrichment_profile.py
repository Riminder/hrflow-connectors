import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.core.auth import APIKeyAuth
from hrflow_connectors.connectors.destinations.flatchr.actions import EnrichProfile
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def credentials(pytestconfig):
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def auth(credentials):
    auth = APIKeyAuth(api_key=credentials["flatchr"]["x-api-key"])
    return auth


@pytest.fixture
def hrflow_client(credentials):
    def hrflow_client_func(portal_name="dev-demo"):
        x_api_key = credentials["hrflow"][portal_name]["x-api-key"]
        x_user_email = credentials["hrflow"][portal_name]["x-user-email"]
        client = Hrflow(api_secret=x_api_key, api_user=x_user_email)
        return client

    return hrflow_client_func


def test_PushProfile(auth, hrflow_client):

    profile = Profile(
        key="5746beca5e941a5a55706efd9adfce31f59e6e2b",
        source=Source(key="d42eed17626b7ae3dc05efca363788caef91d44b"),
    )
    action = EnrichProfile(
        auth=auth,
        subdomain="api",
        hrflow_client=hrflow_client(),
        profile=profile,
        vacancy="k0M5O9ylKZnxbQBy",
        compagny="LEZBvp5b4LdMoVmg",
    )
    response = action.execute()
    assert response.get("status_code") == 201
