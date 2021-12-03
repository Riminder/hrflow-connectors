import requests
from pydantic import BaseModel, Field
from typing import Optional, Dict


from .auth import Auth, NoAuth


class HTTPStream(BaseModel):
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

        return self.session.request(
            method=self.http_method,
            url=url,
            params=params,
            headers=headers,
            cookies=cookies,
            data=payload,
        )

    @property
    def session(self) -> requests.Session:
        return self._session
