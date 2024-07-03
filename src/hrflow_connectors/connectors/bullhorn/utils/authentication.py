import json
import urllib.parse as urlparse
import urllib.request as urllib2

import requests

base_url = "https://auth.bullhornstaffing.com/oauth"


class AuthCodeRedirectHandler(urllib2.HTTPRedirectHandler):
    """
    A bare bones redirect handler that pulls the auth code sent back
    by OAuth off the query string of the redirect URI given in the
    Location header.  Does no checking for other errors or bad/missing
    information.
    """

    def http_error_302(self, req, fp, code, msg, headers):
        """Handler for 302 responses that assumes a properly constructed
        OAuth 302 response and pulls the auth code out of the header."""
        qs = urlparse.urlparse(headers["Location"]).query
        auth_code = urlparse.parse_qs(qs)["code"][0]
        return auth_code


def build_auth_code_request(username, password, client_id):
    data = {
        "client_id": client_id,
        "response_type": "code",
        "username": username,
        "password": password,
        "action": "Login",
    }

    encoded = urlparse.urlencode(data).encode("utf-8")
    req = urllib2.Request(url=base_url + "/authorize", data=encoded)

    try:
        urllib2.urlopen(req)
    except urllib2.HTTPError as e:
        if e.code == 307:
            redirect_url = e.headers["Location"]
            req = urllib2.Request(url=redirect_url, data=encoded)
        else:
            raise Exception(f"HTTP Error {e.code}: {e.read()}")
    return req


def get_auth_code(username, password, client_id):
    """
    Helper function to get the authorization code.
    """
    req = build_auth_code_request(username, password, client_id)
    opener = urllib2.build_opener(AuthCodeRedirectHandler)
    return opener.open(req)


def make_token_request(data):
    """
    Helper function to make the token request and handle 307 redirects.
    """
    encoded = urlparse.urlencode(data).encode("utf-8")
    req = urllib2.Request(base_url + "/token", encoded)

    try:
        response = urllib2.urlopen(req)
        return response.read().decode("utf-8")
    except urllib2.HTTPError as e:
        if e.code == 307:
            redirect_url = e.headers["Location"]
            req = urllib2.Request(url=redirect_url, data=encoded)
            response = urllib2.urlopen(req)
            return response.read().decode("utf-8")
        else:
            raise Exception(f"HTTP Error {e.code}: {e.read()}")


def login_to_bullhorn(access_token):
    """
    Helper function to log in to Bullhorn using the access token.
    """
    login_url = "https://rest.bullhornstaffing.com/rest-services/login"
    params = {"version": "2.0", "access_token": access_token["access_token"]}
    response = requests.post(url=login_url, params=params)

    if response.status_code != 200:
        raise Exception(
            f"Login to Bullhorn failed with status code {response.status_code}:"
            f" {response.text}"
        )

    response = response.json()
    response["refresh_token"] = access_token["refresh_token"]
    return response


def get_or_refresh_token(
    grant_type, client_id, client_secret, ttl=None, code=None, refresh_token=None
):
    """
    Gets or refreshes an OAuth access token based on the grant type.
    """
    data = {
        "grant_type": grant_type,
        "client_id": client_id,
        "client_secret": client_secret,
    }
    if grant_type == "authorization_code":
        data["code"] = code
    elif grant_type == "refresh_token":
        data["refresh_token"] = refresh_token

    # Add TTL if specified
    if ttl:
        data["ttl"] = ttl

    token_response = make_token_request(data)
    access_token = json.loads(token_response)

    # Login to Bullhorn and return the response
    return login_to_bullhorn(access_token)


def auth(
    username, password, client_id, client_secret, refresh_token=None, auth_code=None
):
    if refresh_token:
        access_token = get_or_refresh_token(
            "refresh_token",
            client_id,
            client_secret,
            ttl=604800,
            refresh_token=refresh_token,
        )
    elif auth_code:
        access_token = get_or_refresh_token(
            "authorization_code", client_id, client_secret, ttl=604800, code=auth_code
        )  # 7 days in seconds
    else:
        auth_code = get_auth_code(username, password, client_id)
        access_token = get_or_refresh_token(
            "authorization_code", client_id, client_secret, ttl=604800, code=auth_code
        )
    return access_token
