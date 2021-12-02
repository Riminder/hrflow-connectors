import os
import json
import pytest

from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.boards.crosstalent.actions import GetAllJobs

import hrflow_connectors as hc

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


@pytest.fixture
def auth():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())

    access_token_url = "https://test.salesforce.com/services/oauth2/token"

    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=credentials["crosstalent"]["oauth2"]["client_id"],
        client_secret=credentials["crosstalent"]["oauth2"]["client_secret"],
        username=credentials["crosstalent"]["oauth2"]["username"],
        password=credentials["crosstalent"]["oauth2"]["password"],
    )
    return auth


def test_Auth(auth):
    access_token = auth.get_access_token()
    assert isinstance(access_token, str)
    assert access_token != ""


def test_GetAllJobs(auth):
    action = GetAllJobs(auth=auth)
    response = action.send_request()

    assert response.status_code == 200
