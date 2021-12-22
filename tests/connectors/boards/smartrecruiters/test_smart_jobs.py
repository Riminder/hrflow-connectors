import os
import json
import pytest
from hrflow import Hrflow
import hrflow_connectors as hc
from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.boards.smartrecruiters.actions import SmartJobs

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


@pytest.fixture
def credentials():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def hrflow_client(credentials):
    def hrflow_client_func(portal_name="dev-demo"):
        x_api_key = credentials["hrflow"][portal_name]["x-api-key"]
        x_user_email = credentials["hrflow"][portal_name]["x-user-email"]
        client = Hrflow(api_secret=x_api_key, api_user=x_user_email)
        return client

    return hrflow_client_func


def test_SmartJobs(hrflow_client):
    action = SmartJobs(
        token="", 
        updated_after="", 
        offset=1, 
        limit=1
        )
