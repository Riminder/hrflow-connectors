import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors import Crosstalent


@pytest.fixture
def auth(credentials):
    access_token_url = "https://test.salesforce.com/services/oauth2/token"

    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=credentials["crosstalent"]["oauth2"]["client_id"],
        client_secret=credentials["crosstalent"]["oauth2"]["client_secret"],
        username=credentials["crosstalent"]["oauth2"]["username"],
        password=credentials["crosstalent"]["oauth2"]["password"],
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    Crosstalent.pull_jobs(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="8eba188e1af123a9818d00974ff37b943b7d54f4",
        hydrate_with_parsing=True,
    )