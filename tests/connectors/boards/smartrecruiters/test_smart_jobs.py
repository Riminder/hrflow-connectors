import os
import json
import pytest
from hrflow import Hrflow
import hrflow_connectors as hc
from hrflow_connectors.core.auth import SmartToken
from hrflow_connectors.connectors.boards.smartrecruiters.actions import SmartJobs
from hrflow_connectors.utils.logger import get_logger_with_basic_config

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


@pytest.fixture
def credentials():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def auth(credentials):
    auth = SmartToken(
        access_token=credentials["smartrecruiters"]["oauth2"]["X-SmartToken"]
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


def test_SmartJobs(auth, hrflow_client):
    logger = get_logger_with_basic_config()
    action = SmartJobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        limit=2,
        board_key="8ebdea98768dfc04d15f76afab70415ed280ea90",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=False,
        posting_status="PUBLIC"
    )
    action.execute()
