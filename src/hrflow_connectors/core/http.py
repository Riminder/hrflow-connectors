from logging import log
import requests
from pydantic import BaseModel, Field
from typing import Optional, Dict, Any, Union, TypeVar


from .auth import Auth, NoAuth
from ..utils.logger import get_logger


logger = get_logger()


class HTTPStream(BaseModel):
    """
    HTTPStream
    Abstract Class

    To send a request with a JSON body, you must set the `content-type` header to `application/json`.
    It is important to write the header key `content-type` in lower case.
    """

    auth: Auth = NoAuth()

    params: Dict[str, str] = dict()
    headers: Dict[str, str] = dict()
    payload: Union[None, str, Dict[str, Any]] = None
    cookies: Dict[str, str] = dict()

    @property
    def base_url(self) -> Optional[str]:
        return None

    @property
    def http_method(self):
        return "GET"

    def path(self):
        return ""

    def build_request_params(self):
        pass

    def build_request_headers(self):
        pass

    def build_request_payload(self):
        pass

    def build_request_cookies(self):
        pass

    def get_content_type(self) -> Optional[str]:
        """
        Get Content-Type of the body request

        This function find the "Content-Type" in Header
        and it is NOT case sensitive.
        So, it can find "content-type" or "CONTENT-TYPE" as well.

        Returns:
            str: return content-type or None.
        """
        for key, value in self.headers.items():
            if key.lower() == "content-type":
                return value

    def send_request(self) -> requests.Response:
        """
        Send a HTTP request

        Returns:
            requests.Response: request response
        """
        if self.base_url is None:
            logger.error("Base URL `base_url` (property function) is not defined !")
            raise ConnectionError(
                "Base URL `base_url` (property function) is not defined !"
            )

        # Build the request
        url = self.base_url + self.path()
        logger.debug(f"URL use in HTTPStream : `{url}`")

        logger.debug("Preparing request...")
        logger.debug("Preparing request params")
        self.build_request_params()
        logger.debug("Preparing request headers")
        self.build_request_headers()
        logger.debug("Preparing request payload")
        self.build_request_payload()
        logger.debug("Preparing request cookies")
        self.build_request_cookies()
        logger.debug("The request has been prepared")

        # Add the auth property in the different sections
        logger.debug("Adding auth properties in the different request fields...")
        self.auth.update(
            params=self.params,
            headers=self.headers,
            cookies=self.cookies,
            payload=self.payload,
        )
        logger.debug("Auth properties have been added in the request fields")

        params = dict()
        params["method"] = self.http_method
        params["url"] = url
        params["params"] = self.params
        params["headers"] = self.headers
        params["cookies"] = self.cookies

        logger.debug(f"A `{self.http_method}` request will be sent to this URL `{url}`")

        # Check if request is a JSON application
        content_type = self.get_content_type()
        if (
            content_type is not None
            and "application/json" in content_type.lower()
            and isinstance(self.payload, dict)
        ):
            logger.debug("The payload has been detected as JSON")
            params["json"] = self.payload
        else:
            logger.debug("The payload has been detected as String or Dataform")
            params["data"] = self.payload

        logger.debug("Sending the request now")
        return requests.request(**params)
