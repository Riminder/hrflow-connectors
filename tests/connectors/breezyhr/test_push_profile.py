import pytest

from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors import BreezyHr
from hrflow_connectors.utils.hrflow import Profile, Source



@pytest.fixture
def auth(credentials):
    auth = OAuth2EmailPasswordBody(
        access_token_url = "https://api.breezy.hr/v3/signin",
        email = 'limam.vadhel@hrflow.ai',
        password="LIMAMok"
    )
    return auth


def test_PushProfileAction(logger, auth, hrflow_client):
    profile = Profile(
        key="65903f6989594e72ccf64a10875127b1293fdce3",
        source=Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    response = BreezyHr.push_profile(
        auth=auth,
        position_id="edc0330a0f3801",
        company_name="Hrflow.ai",
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    assert response.get("status_code") == 201
