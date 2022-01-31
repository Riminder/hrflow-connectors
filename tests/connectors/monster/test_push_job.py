import pytest

from hrflow_connectors import Monster
from hrflow_connectors import MonsterBodyAuth
from hrflow_connectors.utils.schemas import HrflowJob


@pytest.fixture
def auth(config):
    auth = MonsterBodyAuth(
        username=config.MONSTER_USERNAME,
        password=config.MONSTER_PASSWORD,
    )
    return auth


def test_PushJobBaseAction(logger, auth, hrflow_client):
    job = HrflowJob(
        key="230f841bc57774bde1ad67563d9f092a788364e0",
        board=dict(key="d31518949ed1f88ac61308670324f93bc0f9374d"),
    )
    response = Monster.push_job(
        auth=auth,
        hrflow_client=hrflow_client(),
        job=job,
        subdomain="gateway",
    )
    assert response.get("status_code") == 201
