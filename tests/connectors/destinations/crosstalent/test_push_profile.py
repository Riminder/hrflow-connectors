import os
import json
import pytest
from hrflow import Hrflow

import hrflow_connectors as hc
from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.destinations.crosstalent.actions import PushProfile

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


@pytest.fixture
def credentials():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def auth(credentials):
    access_token_url = "https://test.salesforce.com/services/oauth2/token"

    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=credentials["crosstalent"]["oauth2"]["client_id"],
        client_secret=credentials["crosstalent"]["oauth2"]["client_secret"],
        username=credentials["crosstalent"]["oauth2"]["username"],
        password=credentials["crosstalent"]["oauth2"]["password"],
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
    action = PushProfile(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("vulcain"),
        profile_key="f43920e95fd485bfff0fb4707bfaac927533fa5a",
        source_key="18928d3753a1cb312b3541236599358b94e53957",
    )
    response = action.execute()
    assert response.get("status_code") == 201
