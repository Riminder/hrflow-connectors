import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors import Crosstalent


@pytest.fixture
def auth(config):
    access_token_url = "https://test.salesforce.com/services/oauth2/token"
    auth = OAuth2PasswordCredentialsBody(
        access_token_url=access_token_url,
        client_id=config.CROSSTALENT_CLIENT_ID,
        client_secret=config.CROSSTALENT_CLIENT_SECRET,
        username=config.CROSSTALENT_USERNAME,
        password=config.CROSSTALENT_PASSWORD,
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    Crosstalent.pull_jobs(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="34ba5e086fd953f2bb835c58f2a3bb2425dc09e6",
        hydrate_with_parsing=True,
    )