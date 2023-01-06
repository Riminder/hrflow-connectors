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
        """handler for 302 responses that assumes a properly constructed
        OAuth 302 response and pulls the auth code out of the header"""
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

    encoded = urlparse.urlencode(data)
    encoded = encoded.encode("utf-8")

    req = urllib2.Request(url=base_url + "/authorize", data=encoded)
    return req


def get_access_token(code, client_id, client_secret):
    """
    Gets an OAuth access token given an OAuth authorization code
    """
    data = {
        "grant_type": "authorization_code",
        "client_id": client_id,
        "client_secret": client_secret,
        "code": code,
    }
    encoded = urlparse.urlencode(data)
    encoded = encoded.encode("utf-8")

    req = urllib2.Request(base_url + "/token", encoded)
    return urllib2.urlopen(req).read()


def auth(username, password, client_id, client_secret):
    req = build_auth_code_request(username, password, client_id)
    opener = urllib2.build_opener(AuthCodeRedirectHandler)

    auth_code = opener.open(req)
    access_token = get_access_token(auth_code, client_id, client_secret)
    access_token = access_token.decode("utf-8")
    access_token = json.loads(access_token)

    login_url = "https://rest.bullhornstaffing.com/rest-services/login"
    params = {"version": "2.0", "access_token": access_token["access_token"]}

    response = requests.post(url=login_url, params=params)
    response = response.json()
    response["refresh_token"] = access_token["refresh_token"]

    return response
