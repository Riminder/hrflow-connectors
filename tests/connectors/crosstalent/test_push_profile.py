import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors import Crosstalent
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(config):
    access_token_url = "https://test.salesforce.com/services/oauth2/token"
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=config.CROSSTALENT_CLIENT_ID,
        client_secret=config.CROSSTALENT_CLIENT_SECRET,
        username=config.CROSSTALENT_USERNAME,
        password=config.CROSSTALENT_PASSWORD,
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
