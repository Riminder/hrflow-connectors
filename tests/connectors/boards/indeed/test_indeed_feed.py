import os
import json
import pytest
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.indeed.actions import IndeedFeed


@pytest.fixture
def credentials(pytestconfig):
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture
def hrflow_client(credentials):
    x_api_key = credentials["hrflow"]["x-api-key"]
    x_user_email = credentials["hrflow"]["x-user-email"]
    client = Hrflow(api_secret=x_api_key, api_user=x_user_email)
    return client


def test_IndeedFeed(hrflow_client, pytestconfig):
    executable_path = os.path.join(
        pytestconfig.rootpath,
        "src/hrflow_connectors/connectors/boards/indeed/chromedriver_linux64/chromedriver",
    )
    action = IndeedFeed(
        executable_path=executable_path,
        max_page=2,
        subdomain="fr",
        hrflow_client=hrflow_client,
        job_search="Software Engineer",
        job_location="Paris",
        board_key="5865a71e45b94e29f7c1c97d71479ef2757df414",
        hydrate_with_parsing=True,
        archive_deleted_jobs_from_stream=False,
    )
    action.execute()
