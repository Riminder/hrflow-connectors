import requests
import responses
from typing import Dict

from hrflow_connectors.core.http import HTTPAction
from hrflow_connectors.core.auth import Auth, NoAuth


@responses.activate
def test_HTTPAction_get():
    request_url = "https://test.test/get"
    body = {"test": "hello"}

    # build Mock for request
    responses.add(responses.GET, request_url, status=200, json=body)

   # HTTPAction to test
    class TestHTTPAction(HTTPAction):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "GET"
        
        @property
        def url_base(self):
            return "https://test.test"
        
        def path(self):
            return "/get"

    action = TestHTTPAction()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"


@responses.activate
def test_HTTPAction_post():
    request_url = "https://test.test/post"
    body = {"test": "hello"}

    # build Mock for request
    responses.add(responses.POST, request_url, status=200, json=body)

    # HTTPAction to test
    class TestHTTPAction(HTTPAction):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "POST"
        
        @property
        def url_base(self):
            return "https://test.test"
        
        def path(self):
            return "/post"

    action = TestHTTPAction()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"


@responses.activate
def test_HTTPAction_get_with_header():
    request_url = "https://test.test/header"
    body = {"test": "hello"}

    # build Mock for request
    match = [responses.matchers.header_matcher(dict(d="world !"))]
    responses.add(responses.GET, request_url, status=200, json=body, match=match)

    # HTTPAction to test
    class TestHTTPAction(HTTPAction):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "GET"
        
        @property
        def url_base(self):
            return "https://test.test"
        
        def path(self):
            return "/header"
        
        def build_request_headers(self):
            super().build_request_headers()
            self._headers["d"] = "world !"

    action = TestHTTPAction()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"
