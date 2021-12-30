import os
import json
import pytest
from hrflow import Hrflow

import hrflow_connectors as hc
from hrflow_connectors.core.auth import SmartToken
from hrflow_connectors.connectors.destinations.smartrecruiters.actions import (
    SmartCandidate,
)
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def credentials(pytestconfig):
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
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


def test_SmartCandidate(auth, hrflow_client):

    profile = Profile(
        key="89ddf5f18768747011a06b8921607cb54a4274a5",
        source=Source(key="6d68a20b2dd7c2bfdcb232b9234c38eada0fdcb4"),
    )
    action = SmartCandidate(
        auth=auth,
        job_uuid="3696cad0-a9b0-4a40-9cd7-4cc5feb1a509",
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    response = action.execute()
    assert response.get("status_code") == 201