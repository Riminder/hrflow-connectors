import responses

from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.core.auth import APIKeyAuth
from hrflow_connectors.core.auth import SmartToken

OAuth2PasswordCredentialsBody_JSON_RESPONSE = {
    "access_token": "ABC.TOKEN.EFD",
    "instance_url": "https://test.test/services/oauth2/token",
    "id": "https://test.test/id/00D3N0000002eRJXAY/0055N000006PGpLQAW",
    "token_type": "Bearer",
    "issued_at": "1638442809550",
    "signature": "wp1jFqVjffN2gLP3O9NvK93VBYFdgHD/FtwiYEhPrlQ=",
}


@responses.activate
def test_APIKeyAuth():

    APIKey = "im_bond_james_bond"
    auth = APIKeyAuth(api_key=APIKey)

    headers = dict(test="abc")
    auth.update(headers=headers)

    assert headers.get("test") == "abc"
    assert headers.get("Authorization") == APIKey


@responses.activate
def test_OAuth2PasswordCredentialsBody_get_access_token():
    access_token_url = "https://test.test/services/oauth2/token"
    client_id = "007"
    client_secret = "double0"
    username = "bond"
    password = "jb"

    # build Mock for request
    body = dict(
        grant_type="password",
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )
    match = [responses.matchers.urlencoded_params_matcher(body)]

    responses.add(
        responses.POST,
        access_token_url,
        status=200,
        json=OAuth2PasswordCredentialsBody_JSON_RESPONSE,
        match=match,
    )

    # Auth to test
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )

    access_token = auth.get_access_token()
    assert access_token == OAuth2PasswordCredentialsBody_JSON_RESPONSE["access_token"]

    headers = dict(test="abc")
    auth.update(headers=headers)

    assert headers.get("test") == "abc"
    assert headers.get("Authorization") == "OAuth {}".format(access_token)


def test_SmartToken():
    auth = SmartToken(access_token="abc")
    assert auth.get_access_token() == "abc"

    headers = dict(test="smart")
    auth.update(headers=headers)
    assert headers == {"test": "smart", "X-SmartToken": "abc"}
