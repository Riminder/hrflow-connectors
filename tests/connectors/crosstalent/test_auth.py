import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody


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


def test_Auth(auth):
    access_token = auth.get_access_token()
    assert isinstance(access_token, str)
    assert access_token != ""