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
    x_api_key = credentials["hrflow"]["x-api-key"]
    x_user_email = credentials["hrflow"]["x-user-email"]
    client = Hrflow(api_secret=x_api_key, api_user=x_user_email)
    return client


def test_SmartJobs(hrflow_client):
    action = SmartJobs(
        token="",
        hrflow_client=hrflow_client,  
        offset=0, 
        limit=2,
        board_key = "",
        hydrate_with_parsing=True,
        archive_deleted_jobs_from_stream=False,

        )

    action.execute()
