import pytest

from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors import Crosstalent
from hrflow_connectors.utils.schemas import HrflowProfile


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


def test_PushProfileBaseAction(logger, auth, hrflow_client):
    profile = HrflowProfile(
        key="a7e7fa4af68e7c450f2b708d14a3bda9b6ade5d9",
        source=dict(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    response = Crosstalent.push_profile(
        auth=auth,
        subdomain="vulcain-eng--recette.my",
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    assert response.get("status_code") == 201
