from urllib.parse import parse_qs, urlparse

import requests

base_url = "https://auth.bullhornstaffing.com/oauth"


def get_auth_code(username, password, client_id):
    """
    Retrieve the authorization code by initiating the OAuth flow.
    """
    data = {
        "client_id": client_id,
        "response_type": "code",
        "username": username,
        "password": password,
        "action": "Login",
    }
    authorize_url = base_url + "/authorize"
    response = requests.post(authorize_url, data=data, allow_redirects=True)
    if response.ok:
        redirect_url = response.url
        parsed_url = urlparse(redirect_url)
        auth_code = parse_qs(parsed_url.query)["code"][0]
        return auth_code
    raise Exception(
        f"Authorization failed with status code {response.status_code}: {response.text}"
    )


def make_token_request(data):
    """
    Make a request to obtain the OAuth access token.
    """
    token_url = base_url + "/token"
    response = requests.post(token_url, data=data)
    if response.ok:
        return response.json()

    raise Exception(
        f"Token request failed with status code {response.status_code}: {response.text}"
    )


def login_to_bullhorn(access_token):
    """
    Log in to Bullhorn using the obtained access token.
    """
    login_url = "https://rest.bullhornstaffing.com/rest-services/login"
    params = {"version": "2.0", "access_token": access_token["access_token"]}
    response = requests.post(url=login_url, params=params)

    if response.ok:
        auth_response = response.json()
        auth_response["refresh_token"] = access_token["refresh_token"]
        return auth_response

    raise Exception(
        f"Login to Bullhorn failed with status code {response.status_code}:"
        f" {response.text}"
    )


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
    # Login to Bullhorn and return the response
    return login_to_bullhorn(token_response)


def auth(
    username, password, client_id, client_secret, refresh_token=None, auth_code=None
):
    """
    Obtain the access token for authentication.
    """
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
