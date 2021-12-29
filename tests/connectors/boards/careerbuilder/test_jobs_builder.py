import os
import json
import pytest
from hrflow import Hrflow
import hrflow_connectors as hc
from hrflow_connectors.connectors.boards.careerbuilder.actions import JobsBuilder
from hrflow_connectors.utils.logger import get_logger_with_basic_config
#adding web driver manager as a DEV dependency to make testing easier for users
from webdriver_manager.chrome import ChromeDriverManager

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

def test_JobsBuilder(hrflow_client):
    logger = get_logger_with_basic_config()
    action = JobsBuilder(
        executable_path=ChromeDriverManager().install(),
        domain='fr',
        hrflow_client=hrflow_client("dev-demo"),
        job_search='Data Scientist',
        job_location='Paris',
        hydrate_with_parsing=True,
        archive_deleted_jobs_from_stream=False,
        board_key="34ac9c1f449ea94ca027882c68098fdb4efa987f",
        )
    action.execute()