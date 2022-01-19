import pytest

from hrflow_connectors import Monster
from hrflow_connectors import MonsterBodyAuth
from hrflow_connectors.utils.hrflow import Job, Board


@pytest.fixture
def auth(credentials):
    auth = MonsterBodyAuth(
        username=credentials["monster"]["username"],
        password=credentials["monster"]["password"],
    )
    return auth


def test_PushJobBaseAction(logger, auth, hrflow_client):
    job = Job(
        key="230f841bc57774bde1ad67563d9f092a788364e0",
        board=Board(key="d31518949ed1f88ac61308670324f93bc0f9374d"),
    )
    response = Monster.push_job(
        auth=auth,
        hrflow_client=hrflow_client(),
        job=job,
        subdomain="gateway",
    )
    assert response.get("status_code") == 201
