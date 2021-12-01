import requests

from .auth import Auth, NoAuth

class HTTPAction:
    def __init__(self, auth: Auth = NoAuth):
        self._auth = auth
        self._session = requests.Session()

        self.url = None
        self.http_method = "GET"

        self.params = dict()
        self.headers = dict()
        self.cookies = dict()
        self.payload = dict()

    def execute(self) -> requests.Response:
        if self.url is None:
            raise ConnectionError("URL is not defined !")

        params = self.params
        if params == dict():
            params = None
        
        headers = self.headers
        if headers == dict():
            headers = None

        cookies = self.cookies
        if cookies == dict():
            cookies = None

        payload = self.payload
        if payload == dict():
            payload = None

        return self.session.request(method=self.http_method, url=self.url, params=params, headers=headers, cookies=cookies, data=payload)

    @property
    def auth(self) -> Auth:
        return self._auth
    
    @property
    def session(self) -> requests.Session:
        return self._session