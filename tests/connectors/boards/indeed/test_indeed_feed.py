import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.indeed.actions import IndeedFeed
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


def test_IndeedFeed(hrflow_client):
    logger = get_logger_with_basic_config()
    action = IndeedFeed(
        executable_path=ChromeDriverManager().install(),
        max_page=2,
        subdomain="fr",
        hrflow_client=hrflow_client("dev-demo"),
        job_search="Software Engineer",
        job_location="Paris",
        board_key="5865a71e45b94e29f7c1c97d71479ef2757df414",
        hydrate_with_parsing=True,
    )
    action.execute()
