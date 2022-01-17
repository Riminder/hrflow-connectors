import pytest

from hrflow_connectors import OAuth2Session
from hrflow_connectors import Bullhorn
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(credentials):
    auth = OAuth2Session(auth_code_url="https://auth.bullhornstaffing.com/oauth/authorize",
                         access_token_url="https://auth.bullhornstaffing.com/oauth/token",
                         session_token_url="https://rest.bullhornstaffing.com/rest-services/login",
                         client_id=credentials["bullhorn"]["client_id"],
                         client_secret=credentials["bullhorn"]["client_secret"],
                         username=credentials["bullhorn"]["username"],
                         password=credentials["bullhorn"]["password"],
                         name="BhRestToken")
    return auth


def test_PushProfile(logger, auth, hrflow_client):
    profile = Profile(
        key="5574b4ebdfe0e52446eade74e87cfe04739d1c96",
        source=Source(key="af00e468b1cf0d5eda0bc6062f2e163d50b1872e"),
    )
    print(type(profile))
    response = Bullhorn.push_profile(
        auth=auth,
        subdomain="rest91",
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    assert response.get("status_code") == 201
