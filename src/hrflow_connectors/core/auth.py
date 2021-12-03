import requests
from typing import Union, Dict, Optional
from pydantic import BaseModel, validator


class Auth(BaseModel):
    def update(
        self,
        params: Union[Dict[str, str], None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[Dict[str, str], None] = None,
        cookies: Union[Dict[str, str], None] = None,
    ):
        pass


class NoAuth(Auth):
    pass


class OAuth2PasswordCredentialsBody(Auth):
    access_token_url: str
    client_id: str
    client_secret: str
    username: str
    password: str

    def get_access_token(self):
        payload = dict()
        payload["grant_type"] = "password"
        payload["client_id"] = self.client_id
        payload["client_secret"] = self.client_secret
        payload["username"] = self.username
        payload["password"] = self.password

        response = requests.post(self.access_token_url, data=payload)

        if response.status_code != 200:
            raise ConnectionError(
                "OAuth2 failed ! Reason : `{}`".format(response.content)
            )
        return response.json()["access_token"]

    def update(
        self,
        params: Union[Dict[str, str], None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[Dict[str, str], None] = None,
        cookies: Union[Dict[str, str], None] = None,
    ):
        if headers is not None:
            headers.update(
                {"Authorization": "OAuth {}".format(self.get_access_token())}
            )
