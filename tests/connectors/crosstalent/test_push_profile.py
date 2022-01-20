import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors import Crosstalent
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(credentials):
    access_token_url = "https://test.salesforce.com/services/oauth2/token"
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=credentials["crosstalent"]["oauth2"]["client_id"],
        client_secret=credentials["crosstalent"]["oauth2"]["client_secret"],
        username=credentials["crosstalent"]["oauth2"]["username"],
        password=credentials["crosstalent"]["oauth2"]["password"],
    )
    return auth


def test_PushProfileBaseAction(logger, auth, hrflow_client):
    profile = Profile(
        key="ea5704b959c5e53aaef65c04ef5018ae1fee1a77",
        source=Source(key="15517d70b0870e4cf431eefd78f8b39cff5607e8"),
    )
    response = Crosstalent.push_profile(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("vulcain"),
        profile=profile,
    )
    assert response.get("status_code") == 201
