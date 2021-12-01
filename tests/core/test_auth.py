import os
import json

import hrflow_connectors as hc
from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody

ROOT_PATH = os.path.abspath(os.path.join(os.path.dirname(hc.__file__), "../../"))


def get_credentials():
    with open(os.path.join(ROOT_PATH, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials["crosstalent"]["oauth2"]


def test_OAuth2PasswordCredentialsBody_get_access_token():
    credentials = get_credentials()
    access_token_url = "https://test.salesforce.com/services/oauth2/token"
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url, **credentials
    )

    assert isinstance(auth.access_token, str)
    assert auth.access_token != ""

    headers = dict(test="abc")
    auth.update(headers=headers)

    assert headers.get("test") == "abc"
    assert headers.get("Authorization") == "OAuth {}".format(auth.access_token)
