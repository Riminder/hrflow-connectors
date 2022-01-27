import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody


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


def test_Auth(auth):
    access_token = auth.get_access_token()
    assert isinstance(access_token, str)
    assert access_token != ""