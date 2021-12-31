import requests
from typing import Union, Dict, Optional
from pydantic import BaseModel, Field


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


class XAPIKeyAuth(Auth):
    """
    X-API-Key Auth

    Auth used to authenticate with an `X-API-Key` in the headers
    """

    name: str = Field(
        "X-API-KEY",
        description="Name of the API key that will be used as a header in the request. Default value : `X-API-KEY`",
    )
    value: str = Field(..., description="API key value")

    def update(
        self,
        params: Union[Dict[str, str], None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[Dict[str, str], None] = None,
        cookies: Union[Dict[str, str], None] = None,
    ):
        if headers is not None:
            headers.update({self.name: self.value})


class AuthorizationAuth(XAPIKeyAuth):
    """
    Authorization Auth

    Auth used to authenticate with an API key named `Authorization` in the headers
    """

    name: str = Field("Authorization", const=True)


class XSmartTokenAuth(XAPIKeyAuth):
    """
    XSmart Token Auth

    Auth used to authenticate to SmartRecruiter with a token
    """

    name: str = Field("X-SmartToken", const=True)