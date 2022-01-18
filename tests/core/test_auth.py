import responses
import requests


from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.core.auth import (
    Auth,
    XAPIKeyAuth,
    AuthorizationAuth,
    XSmartTokenAuth,
    XTaleezAuth,
    OAuth2Session,
)


def test_Auth():
    auth = Auth()
    updatable_object = dict(abc="def")
    updated_object = auth(updatable_object)
    assert updatable_object == updated_object


@responses.activate
def test_OAuth2PasswordCredentialsBody_get_access_token():

    OAuth2PasswordCredentialsBody_JSON_RESPONSE = {
        "access_token": "ABC.TOKEN.EFD",
        "instance_url": "https://test.test/services/oauth2/token",
        "id": "https://test.test/id/00D3N0000002eRJXAY/0055N000006PGpLQAW",
        "token_type": "Bearer",
        "issued_at": "1638442809550",
        "signature": "wp1jFqVjffN2gLP3O9NvK93VBYFdgHD/FtwiYEhPrlQ=",
    }

    access_token_url = "https://test.test/services/oauth2/token"
    check_auth_url = "http://test.test/check_auth"
    client_id = "007"
    client_secret = "double0"
    username = "bond"
    password = "jb"

    # build Mock for get_access_token request
    body = dict(
        grant_type="password",
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )
    get_access_match = [responses.matchers.urlencoded_params_matcher(body)]
    responses.add(
        responses.POST,
        access_token_url,
        status=200,
        json=OAuth2PasswordCredentialsBody_JSON_RESPONSE,
        match=get_access_match,
    )

    # Instanciate Auth to test
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )

    access_token = auth.get_access_token()
    assert access_token == OAuth2PasswordCredentialsBody_JSON_RESPONSE["access_token"]

    # build Mock for check_auth request
    header = dict(Authorization=f"OAuth {access_token}")
    check_auth_match = [responses.matchers.header_matcher(header)]
    responses.add(
        responses.GET,
        check_auth_url,
        status=200,
        match=check_auth_match,
    )

    # Send authenticated request
    session = requests.Session()
    request = requests.Request(
        method="GET", url="http://test.test/check_auth", auth=auth
    )
    prepared_request = request.prepare()
    session.send(prepared_request)

    prepared_request
    prepared_request.headers.update(dict(test="abc"))
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    assert authenticated_prepared_request.headers.get(
        "Authorization"
    ) == "OAuth {}".format(access_token)


@responses.activate
def test_OAuth2PasswordCredentialsBody_get_access_token_failure():

    OAuth2PasswordCredentialsBody_JSON_RESPONSE = {
        "access_token": "ABC.TOKEN.EFD",
        "instance_url": "https://test.test/services/oauth2/token",
        "id": "https://test.test/id/00D3N0000002eRJXAY/0055N000006PGpLQAW",
        "token_type": "Bearer",
        "issued_at": "1638442809550",
        "signature": "wp1jFqVjffN2gLP3O9NvK93VBYFdgHD/FtwiYEhPrlQ=",
    }

    access_token_url = "https://test.test/services/oauth2/token"
    check_auth_url = "http://test.test/check_auth"
    client_id = "007"
    client_secret = "double0"
    username = "bond"
    password = "jb"

    # build Mock for get_access_token request
    body = dict(
        grant_type="password",
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )
    get_access_match = [responses.matchers.urlencoded_params_matcher(body)]
    responses.add(
        responses.POST,
        access_token_url,
        status=400,
        json=OAuth2PasswordCredentialsBody_JSON_RESPONSE,
        match=get_access_match,
    )

    # Instanciate Auth to test
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
    )

    try:
        access_token = auth.get_access_token()
        assert False
    except RuntimeError:
        pass


@responses.activate
def test_XAPIKeyAuth():

    key = "efg"
    auth = XAPIKeyAuth(name="XSuperKey", value=key)

    base_headers = dict(test="abc")

    request = requests.Request(
        method="GET", url="http://test.test/check_auth", headers=base_headers
    )
    prepared_request = request.prepare()
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    assert authenticated_prepared_request.headers.get("XSuperKey") == key
    assert len(authenticated_prepared_request.headers) == 2


@responses.activate
def test_AuthorizationAuth():
    key = "im_bond_james_bond"
    auth = AuthorizationAuth(value=key)

    base_headers = dict(test="abc")

    request = requests.Request(
        method="GET", url="http://test.test/check_auth", headers=base_headers
    )
    prepared_request = request.prepare()
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    assert authenticated_prepared_request.headers.get("Authorization") == key
    assert len(authenticated_prepared_request.headers) == 2


def test_XSmartTokenAuth():
    key = "abc"
    auth = XSmartTokenAuth(value=key)

    base_headers = dict(test="abc")

    request = requests.Request(
        method="GET", url="http://test.test/check_auth", headers=base_headers
    )
    prepared_request = request.prepare()
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    assert authenticated_prepared_request.headers.get("X-SmartToken") == key
    assert len(authenticated_prepared_request.headers) == 2

def test_XTaleezAuth():
    key = "abc"
    auth = XTaleezAuth(value=key)

    base_headers = dict(test="abc")

    request = requests.Request(
        method="GET", url="http://test.test/check_auth", headers=base_headers
    )
    prepared_request = request.prepare()
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    assert authenticated_prepared_request.headers.get("X-taleez-api-secret") == key
    assert len(authenticated_prepared_request.headers) == 2


@responses.activate
def test_OAuth2Session_get_access_token():

    OAuth2Session_JSON_RESPONSE = {
        "auth_code": "007",
        "access_token": "ceciestlacesstoken",
        "BhRestToken": "ceciestlesessiontoken",
        "token_type": "Bearer",
        "expires_in": 600,
        "refresh_token": "refreshtoken"
    }

    auth_code_url = "https://www.site.com/"
    access_token_url = "http://test.test/access_token"
    session_token_url = "http://test.test/session_token"

    check_auth_url = "http://test.test/check_auth"
    client_id = "007"
    client_secret = "double0"
    username = "bond"
    password = "jb"

    # Instanciate Auth to test
    auth = OAuth2Session(
        auth_code_url=auth_code_url,
        access_token_url=access_token_url,
        session_token_url=session_token_url,
        client_id=client_id,
        client_secret=client_secret,
        username=username,
        password=password,
        name="BhRestToken",
    )

    # build Mock for get_access_token request
    body_auth_code = dict(
        client_id=client_id,
        response_type="code",
        action="Login",
        username=username,
        password=password
    )
    get_access_match = [responses.matchers.query_param_matcher(body_auth_code)]
    responses.add(
        responses.POST,
        auth_code_url,
        match=get_access_match,
    )

    auth_code = auth.get_auth_code()
    assert auth_code == OAuth2Session_JSON_RESPONSE["auth_code"]




    # build Mock for get_access_token request
    body_access_token = dict(
        grant_type="authorization_code",
        code=auth_code,
        client_id=client_id,
        client_secret=client_secret,
    )

    get_access_match = [responses.matchers.query_param_matcher(body_access_token)]
    responses.add(
        responses.POST,
        access_token_url,
        status=200,
        json=OAuth2Session_JSON_RESPONSE,
        match=get_access_match,
    )

    access_token = auth.get_access_token(auth_code)
    assert access_token == OAuth2Session_JSON_RESPONSE["access_token"]






    # build Mock for get_access_token request
    body_session_token = dict(
        version="*",
        access_token=access_token
    )

    get_access_match = [responses.matchers.query_param_matcher(body_session_token)]
    responses.add(
        responses.POST,
        session_token_url,
        status=200,
        json=OAuth2Session_JSON_RESPONSE,
        match=get_access_match,
    )

    session_token = auth.get_session_token(access_token)
    print(f"AHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH {session_token}")
    assert session_token["BhRestToken"] == OAuth2Session_JSON_RESPONSE["BhRestToken"]





    # build Mock for check_auth request
    header = dict(BhRestToken=session_token["BhRestToken"])
    check_auth_match = [responses.matchers.header_matcher(header)]
    responses.add(
        responses.GET,
        check_auth_url,
        status=200,
        match=check_auth_match,
    )

    # Send authenticated request
    session = requests.Session()
    request = requests.Request(
        method="GET", url=check_auth_url, auth=auth
    )
    prepared_request = request.prepare()
    session.send(prepared_request)

    prepared_request.headers.update(dict(test="abc"))
    authenticated_prepared_request = auth(prepared_request)

    assert authenticated_prepared_request.headers.get("test") == "abc"
    """assert authenticated_prepared_request.headers.get(
        "Authorization"
    ) == "OAuth {}".format(access_token)"""
