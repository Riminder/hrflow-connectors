import requests
from pydantic import BaseModel
from typing import Optional, Dict


from .auth import Auth, NoAuth


class HTTPAction(BaseModel):
    auth: Auth = NoAuth()
    
    url: Optional[str] = None
    http_method: str = "GET"

    _session: requests.Session = requests.Session()
    _params: Dict[str,str] = dict()
    _headers: Dict[str,str] = dict()
    _cookies: Dict[str,str] = dict()
    _payload: Dict[str,str] = dict()

    def execute(self) -> requests.Response:
        if self.url is None:
            raise ConnectionError("URL is not defined !")

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
            url=self.url,
            params=params,
            headers=headers,
            cookies=cookies,
            data=payload,
        )
    
    @property
    def session(self) -> requests.Session:
        return self._session
