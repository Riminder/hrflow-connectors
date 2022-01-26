import responses
import requests
from unittest import mock

from hrflow_connectors.core.error import AuthError
from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody, OAuth2EmailPasswordBody
from hrflow_connectors.core.auth import (
    Auth,
    XAPIKeyAuth,
    AuthorizationAuth,
    XSmartTokenAuth,
    MonsterBodyAuth,
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
    except AuthError:
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

@responses.activate
def test_OAuth2EmailPasswordBody_get_access_token():

    OAuth2EmailPasswordBody_JSON_RESPONSE = {
        "access_token": "ABC.TOKEN.EFD",
        "instance_url": "https://test.test/services/signon/token",
        "user": {'_id':'fhfh453f','email_address':'lemzo@hotemail.ww','name':'lemzo','usernme':'lemzo30','initial':'L'}
    }

    access_token_url = "https://test.test/services/signon/token"
    check_auth_url = "http://test.test/check_auth"
    email = "lemzo@hotemail.ww"
    password = "jb"

    # build Mock for get_access_token request
    body = dict(
        email=email,
        password=password,
    )
    get_access_match = [responses.matchers.urlencoded_params_matcher(body)]
    responses.add(
        responses.POST,
        access_token_url,
        status=200,
        json=OAuth2EmailPasswordBody_JSON_RESPONSE,
        match=get_access_match,
    )

    # Instanciate Auth to test
    auth = OAuth2EmailPasswordBody(
        access_token_url=access_token_url,
        email=email,
        password=password,
    )

    access_token = auth.get_access_token()
    assert access_token == OAuth2EmailPasswordBody_JSON_RESPONSE["access_token"]

    # build Mock for check_auth request
    header = dict(Authorization=f"{access_token}")
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
    ) == "{}".format(access_token)

@responses.activate
def test_OAuth2EmailPasswordBody_get_access_token_failure():

    OAuth2EmailPasswordBody_JSON_RESPONSE = {
        "access_token": "ABC.TOKEN.EFD",
        "instance_url": "https://test.test/services/signon/token",
        "user": {'_id':'fhfh453f','email_address':'lemzo@hotemail.ww','name':'lemzo','usernme':'lemzo30','initial':'L'}
    }

    access_token_url = "https://test.test/services/signon/token"
    check_auth_url = "http://test.test/check_auth"
    email = "lemzo@hotemail.ww"
    password = "jb"

    # build Mock for get_access_token request
    body = dict(
        email=email,
        password=password,
    )
    get_access_match = [responses.matchers.urlencoded_params_matcher(body)]
    responses.add(
        responses.POST,
        access_token_url,
        status=400,
        json=OAuth2EmailPasswordBody_JSON_RESPONSE,
        match=get_access_match,
    )

    # Instanciate Auth to test
    auth = OAuth2EmailPasswordBody(
        access_token_url=access_token_url,
        email=email,
        password=password,
    )
    try:
        access_token = auth.get_access_token()
        assert False
    except AuthError:
        pass


def test_MonsterBodyAuth():

    username = "efg"
    password = "hij"
    url = "http://test.test/check_auth"
    auth = MonsterBodyAuth(username=username, password=password)
    job = b"<username>{username}</username>\n<password>{password}</password>"

    request = requests.Request(
        method="GET", url=url
    )
    request.data = job

    prepared_request = request.prepare()
    authenticated_prepared_request = auth(prepared_request)

    assert isinstance(authenticated_prepared_request.body, bytes)
    assert authenticated_prepared_request.body == b"<username>efg</username>\n<password>hij</password>"

    
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
def test_OAuth2Session_get_auth_code_success():
    get_auth_code_response = {
        "auth_code": "ceciestlauthcode"
    }
    auth_code_url = "http://test.test/auth_code"
    access_token_url = "http://test.test/access_token"
    session_token_url = "http://test.test/session_token"

    auth_code = "007"
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

    with mock.patch('requests.post') as requests_post_mock:
        # On demande au Mock `requests.get(...).url` de retourner le code
        redirected_url = "http://test.test/login?code={}".format(get_auth_code_response["auth_code"])
        requests_post_mock.return_value.url = redirected_url

        auth_code = auth.get_auth_code()
        assert auth_code == get_auth_code_response["auth_code"]

        # Enfin, on vérifie que la requête envoyée était correcte
        # Pour cela, on va vérifier les appels effectués au Mock
        # `requests_get_mock.mock_calls` donne la liste des appels
        # Par exemple : `[call('http://google.fr'), call().__str__()]`
        # On doit donc vérifier que l'appel à l'api a utilisé les bons paramètres
        body_auth_code = dict(
                client_id=client_id,
                response_type="code",
                action="Login",
                username=username,
                password=password
        )

        expected_call = mock.call(auth_code_url, params=body_auth_code)
        assert expected_call in requests_post_mock.mock_calls


@responses.activate
def test_OAuth2Session_get_auth_code_failure():
    get_auth_code_response = {
        "auth_code": "ceciestlauthcode"
    }
    auth_code_url = "http://test.test/auth_code"
    access_token_url = "http://test.test/access_token"
    session_token_url = "http://test.test/session_token"

    auth_code = "007"
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

    with mock.patch('requests.post') as requests_post_mock:
        # On demande au Mock `requests.get(...).url` de retourner le code
        redirected_url = "http://test.test/login?notcode={}".format(get_auth_code_response["auth_code"])
        requests_post_mock.return_value.url = redirected_url

        try:
            auth.get_auth_code()
            assert False
        except AuthError:
            pass



@responses.activate
def test_OAuth2Session_get_access_token():
    get_access_token_response = {
        "access_token": "ceciestlacesstoken"
    }

    auth_code_url = "http://test.test/auth_code"
    access_token_url = "http://test.test/access_token"
    session_token_url = "http://test.test/session_token"

    auth_code = "007"
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
    get_access_token_request_body = dict(
        grant_type="authorization_code",
        code=auth_code,
        client_id=client_id,
        client_secret=client_secret,
    )

    get_access_match = [responses.matchers.query_param_matcher(get_access_token_request_body)]
    responses.add(
        responses.POST,
        access_token_url,
        status=200,
        json=get_access_token_response,
        match=get_access_match,
    )

    access_token = auth.get_access_token(auth_code)
    assert access_token == get_access_token_response["access_token"]



@responses.activate
def test_OAuth2Session_get_session_token():
    get_access_token_response = {
        "BhRestToken": "ceciestlesessiontoken"
    }

    auth_code_url = "https://www.site.com/"
    access_token_url = "http://test.test/access_token"
    session_token_url = "http://test.test/session_token"

    client_id = "007"
    client_secret = "double0"
    username = "bond"
    password = "jb"
    access_token = "ceciestlacesstoken"

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
    session_token_request_body = dict(
        version="*",
        access_token=access_token
    )

    get_access_match = [responses.matchers.query_param_matcher(session_token_request_body)]
    responses.add(
        responses.POST,
        session_token_url,
        status=200,
        json=get_access_token_response,
        match=get_access_match,
    )

    session_token = auth.get_session_token(access_token)
    assert session_token == get_access_token_response["BhRestToken"]

def test_OAuth2Session_call():
    class MyOAuth2Session(OAuth2Session):
        def get_auth_code(self):
            return "auth_code"
        
        def get_access_token(self, auth_code):
            assert auth_code == "auth_code"
            return "access_token"
        
        def get_session_token(self, access_token):
            assert access_token == "access_token"
            return "session_token"

    auth = MyOAuth2Session(
        auth_code_url="auth_code_url",
        access_token_url="access_token_url",
        session_token_url="session_token_url",
        client_id="client_id",
        client_secret="client_secret",
        username="username",
        password="password",
        name="BhRestToken",
    )

    request = requests.Request(method="POST", url="http://test.test/", headers=dict(abc="efg"))
    prepared_request = request.prepare()
    request_with_auth = auth(prepared_request)
    assert request_with_auth.headers["BhRestToken"] == "session_token"