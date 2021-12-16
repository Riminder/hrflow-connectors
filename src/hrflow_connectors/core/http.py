import requests
from pydantic import BaseModel, Field
from typing import Optional, Dict


from .auth import Auth, NoAuth


class HTTPStream(BaseModel):
    """
    HTTPStream
    Abstract Class

    To send a request with a JSON body, you must set the `content-type` header to `application/json`.
    It is important to write the header key `content-type` in lower case.
    """

    auth: Auth = NoAuth()

    _session: requests.Session = requests.Session()
    _params: Dict[str, str] = dict()
    _headers: Dict[str, str] = dict()
    _payload: Dict[str, str] = dict()
    _cookies: Dict[str, str] = dict()

    @property
    def url_base(self) -> Optional[str]:
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

    def send_request(self) -> requests.Response:
        if self.url_base is None:
            raise ConnectionError("Base URL (property function) is not defined !")

        # Build the request
        url = self.url_base + self.path()
        self.build_request_params()
        self.build_request_headers()
        self.build_request_payload()
        self.build_request_cookies()

        # Add the auth property in the different sections
        self.auth.update(
            params=self._params,
            headers=self._headers,
            cookies=self._cookies,
            payload=self._payload,
        )

        params = self._params
        if params == dict():
            params = None

        headers = self._headers
        if headers == dict():
            headers = None

        cookies = self._cookies
        if cookies == dict():
            cookies = None

        payload = self._payload
        if payload == dict():
            payload = None

        params = dict()
        params["method"] = self.http_method
        params["url"] = url
        params["params"] = params
        params["headers"] = headers
        params["cookies"] = cookies

        # Check if request is a JSON application
        content_type = self._headers.get("content-type")
        if content_type is not None and "application/json" in content_type:
            params["json"] = self._payload
        else:
            params["data"] = self._payload

        return self._session.request(**params)

    @property
    def session(self) -> requests.Session:
        return self._session
