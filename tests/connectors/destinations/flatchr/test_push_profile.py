import os
import json
import pytest
from hrflow import Hrflow

import hrflow_connectors as hc
from hrflow_connectors.core.auth import APIKeyAuth
from hrflow_connectors.connectors.destinations.flatchr.actions import PushProfile
from hrflow_connectors.utils.hrflow import Profile, Source

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


@pytest.fixture
def credentials():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def auth(credentials):
    auth = APIKeyAuth(
        api_key=credentials["crosstalent"]["oauth2"]["client_id"]
    )
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
        key="ea5704b959c5e53aaef65c04ef5018ae1fee1a77",
        source=Source(key="15517d70b0870e4cf431eefd78f8b39cff5607e8"),
    )
    action = PushProfile(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("vulcain"),
        profile=profile,
    )
    response = action.execute()
    assert response.get("status_code") == 201
