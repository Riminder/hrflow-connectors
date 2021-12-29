import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.destinations.crosstalent.actions import PushProfile
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def credentials(pytestconfig):
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
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
