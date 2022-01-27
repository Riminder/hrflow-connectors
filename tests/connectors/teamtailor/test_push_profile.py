import pytest

from hrflow_connectors import AuthorizationAuth
from hrflow_connectors import Teamtailor
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(config):
    return AuthorizationAuth(value=config.TEAMTAILOR_TOKEN)


def test_PushProfile(logger, auth, hrflow_client):

    profile = Profile(
        key="65903f6989594e72ccf64a10875127b1293fdce3",
        source=Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    response = Teamtailor.push_profile(
        company_id="hrflowai",
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    assert response.get("status_code") == 201