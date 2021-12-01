import requests

from hrflow_connectors.core.http import HTTPAction
from hrflow_connectors.core.auth import Auth, NoAuth

def test_HTTPAction_get():
    class TestHTTPAction(HTTPAction):
        def __init__(self, auth:Auth = NoAuth()):
            super().__init__(auth=auth, http_method = "GET", url="https://gorest.co.in/public/v1/posts")
    
    action = TestHTTPAction()
    response = action.execute()

    assert isinstance(response, requests.Response)
    assert response.status_code == 200
    assert "application/json" in response.headers.get('content-type')

    response_json = response.json()
    assert response_json.get("meta") != None
    assert response_json.get("data") != None