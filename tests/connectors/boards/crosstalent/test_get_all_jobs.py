import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.boards.crosstalent.actions import GetAllJobs
from hrflow_connectors.utils.logger import get_logger_with_basic_config


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


def test_Auth(auth):
    access_token = auth.get_access_token()
    assert isinstance(access_token, str)
    assert access_token != ""


def test_GetAllJobs(auth, hrflow_client):
    logger = get_logger_with_basic_config()
    action = GetAllJobs(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="8eba188e1af123a9818d00974ff37b943b7d54f4",
        hydrate_with_parsing=True,
    )
    action.execute()