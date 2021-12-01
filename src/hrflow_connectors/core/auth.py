import requests
from typing import Union, Dict

class Auth:
    def update(
        self,
        url: Union[str, None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[str, None] = None,
        cookies: Union[str, None] = None,
    ):
        pass

class NoAuth(Auth):
    pass

class OAuth2PasswordCredentialsBody(Auth):
    def __init__(
        self,
        access_token_url: str,
        client_id: str,
        client_secret: str,
        username: str,
        password: str,
    ):
        payload = dict()
        payload["grant_type"] = "password"
        payload["client_id"] = client_id
        payload["client_secret"] = client_secret
        payload["username"] = username
        payload["password"] = password

        response = requests.post(access_token_url, data=payload)
        if response.status_code != 200:
            raise ConnectionError(
                "OAuth2 failed ! Reason : `{}`".format(response.content)
            )

        self.access_token = response.json()["access_token"]

    def update(
        self,
        url: Union[str, None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[str, None] = None,
        cookies: Union[str, None] = None,
    ):

        if headers is not None:
            headers.update({"Authorization": "OAuth {}".format(self.access_token)})
