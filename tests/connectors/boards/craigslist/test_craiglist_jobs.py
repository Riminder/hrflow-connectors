import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.craigslist.actions import CraigslistJobs
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# Adding web driver manager as a DEV dependency to make testing easier for users
from webdriver_manager.chrome import ChromeDriverManager


@pytest.fixture
def credentials(pytestconfig):
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
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


def test_CraigslistJobs(hrflow_client):
    logger = get_logger_with_basic_config()
    action = CraigslistJobs(
        executable_path=ChromeDriverManager().install(),
        subdomain="Paris",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="fc197c369ed9a8d35041961b268076885b2ea6f2",
        hydrate_with_parsing=True,
    )
    action.execute()
