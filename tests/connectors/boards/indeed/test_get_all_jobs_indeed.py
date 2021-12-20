import os
import json
import pytest
from hrflow import Hrflow

import hrflow_connectors as hc
from hrflow_connectors.connectors.boards.indeed.actions import GetAllJobs

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



def test_GetAllJobs(hrflow_client):
    action = GetAllJobs(
        
        executable_path = "/limam_ala/projects/indeed/hrflow-connectors/src/hrflow_connectors/connectors/boards/indeed/chromedriver_linux64/chromedriver",
        subdomain="fr",
        hrflow_client=hrflow_client,
        job_search = 'Data Scientist',
        job_location = 'Paris',
        limit = 15,
        limit_extract = 1,
        

        board_key="ad9bdfb905566e88ace7698736efb2f625f65a39",
        hydrate_with_parsing=True,
        archive_deleted_jobs_from_stream = False

    )
    action.execute()