import pytest

from hrflow_connectors import OAuth2Session
from hrflow_connectors import Bullhorn
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(config):
    auth = OAuth2Session(auth_code_url="https://auth.bullhornstaffing.com/oauth/authorize",
                         access_token_url="https://auth.bullhornstaffing.com/oauth/token",
                         session_token_url="https://rest.bullhornstaffing.com/rest-services/login",
                         client_id=config.BULLHORN_CLIENT_ID,
                         client_secret=config.BULLHORN_CLIENT_SECRET,
                         username=config.BULLHORN_USERNAME,
                         password=config.BULLHORN_PASSWORD,
                         name="BhRestToken")
    return auth


def test_PushProfile(logger, auth, hrflow_client):
    profile = Profile(
        key="5574b4ebdfe0e52446eade74e87cfe04739d1c96",
        source=Source(key="af00e468b1cf0d5eda0bc6062f2e163d50b1872e"),
    )
    response = Bullhorn.push_profile(
        auth=auth,
        subdomain="rest91",
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    assert response.get("status_code") == 201
