import requests
from typing import Union, Dict, Optional
from pydantic import BaseModel, Field
from ..utils.logger import get_logger


logger = get_logger()


class Auth(BaseModel):
    """
    Auth

    This generic class will propose a common interface to all Auth
    """

    def update(
        self,
        params: Union[Dict[str, str], None] = None,
        headers: Union[Dict[str, str], None] = None,
        payload: Union[Dict[str, str], None] = None,
        cookies: Union[Dict[str, str], None] = None,
    ):
        """
        Updating fields with authentication information

        This class is responsible for placing the credentials in the right places

        Args:
            params (Union[Dict[str, str], None], optional): Params field. Defaults to None.
            headers (Union[Dict[str, str], None], optional): Headers field. Defaults to None.
            payload (Union[Dict[str, str], None], optional): Payload field. Defaults to None.
            cookies (Union[Dict[str, str], None], optional): Cookies field. Defaults to None.
        """
        pass


class NoAuth(Auth):
    """
    No Auth

    To be used if a class does not require authentication
    """

    pass


class OAuth2PasswordCredentialsBody(Auth):
    """
    OAuth2 by using a password and adding credentials in the body of the request used to get the "access token".
    """

    access_token_url: str
    client_id: str
    client_secret: str
    username: str
    password: str

    def get_access_token(self) -> str:
        """
        Get access token

        Returns:
            str: Access token
        """
        logger.debug("Getting the access token...")
        payload = dict()
        payload["grant_type"] = "password"
        payload["client_id"] = self.client_id
        payload["client_secret"] = self.client_secret
        payload["username"] = self.username
        payload["password"] = self.password

        logger.debug(
            f"Sending request to get access token (url=`{self.access_token_url}`)"
        )
        response = requests.post(self.access_token_url, data=payload)

        if response.status_code != 200:
            logger.error("OAuth2 failed for getting access token !")
            raise ConnectionError(
                "OAuth2 failed ! Reason : `{}`".format(response.content)
            )

        logger.debug("The access token has been got")
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