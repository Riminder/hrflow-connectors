import requests
import responses
from typing import Dict

from hrflow_connectors.core.http import HTTPStream
from hrflow_connectors.core.auth import Auth, NoAuth


@responses.activate
def test_HTTPStream_get():
    request_url = "https://test.test/get"
    body = {"test": "hello"}

    # build Mock for request
    responses.add(responses.GET, request_url, status=200, json=body)

    # HTTPStream to test
    class TestHTTPStream(HTTPStream):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "GET"

        @property
        def base_url(self):
            return "https://test.test"

        def path(self):
            return "/get"

    action = TestHTTPStream()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"


@responses.activate
def test_HTTPStream_post():
    request_url = "https://test.test/post"
    body = {"test": "hello"}

    # build Mock for request
    responses.add(responses.POST, request_url, status=200, json=body)

    # HTTPStream to test
    class TestHTTPStream(HTTPStream):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "POST"

        @property
        def base_url(self):
            return "https://test.test"

        def path(self):
            return "/post"

    action = TestHTTPStream()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"


@responses.activate
def test_HTTPStream_get_with_header():
    request_url = "https://test.test/header"
    body = {"test": "hello"}

    # build Mock for request
    match = [responses.matchers.header_matcher(dict(d="world !"))]
    responses.add(responses.GET, request_url, status=200, json=body, match=match)

    # HTTPStream to test
    class TestHTTPStream(HTTPStream):
        auth: Auth = NoAuth()

        @property
        def http_method(self):
            return "GET"

        @property
        def base_url(self):
            return "https://test.test"

        def path(self):
            return "/header"

        def build_request_headers(self):
            self.headers["d"] = "world !"

    action = TestHTTPStream()
    response = action.send_request()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get("content-type")

    response_json = response.json()
    assert response_json.get("test") == "hello"
