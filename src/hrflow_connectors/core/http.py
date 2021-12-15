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

    To define `params`, `headers`, `payload` or `cookies`, you cannot override the associated private attributes.
    It is necessary to overload the functions `build_request_...`. For example: `build_request_params`, ...
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
        self._params.clear()

    def build_request_headers(self):
        self._headers.clear()

    def build_request_payload(self):
        self._payload.clear()

    def build_request_cookies(self):
        self._cookies.clear()

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
        params["params"] = self.params
        params["headers"] = self.headers
        params["cookies"] = self.cookies

        # Check if request is a JSON application
        content_type = self.headers.get("content-type")
        if content_type is not None and "application/json" in content_type:
            params["json"] = self.payload
        else:
            params["data"] = self.payload

        return self.session.request(**params)

    @property
    def session(self) -> requests.Session:
        return self._session
