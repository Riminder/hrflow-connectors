import pytest
from hrflow_connectors import SapSuccessfactors
from hrflow_connectors import XAPIKeyAuth


@pytest.fixture
def auth(config):
    auth = XAPIKeyAuth(
        name='APIKey',
        value=config.SAPSUCCESSFACTORS_TOKEN,
    )
    return auth

def test_PullJobs(logger,auth, hrflow_client):
    SapSuccessfactors.pull_jobs(
        auth = auth,
        api_server="sandbox.api.sap.com:443/successfactors",
        top=20,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="91c42ec77a7abfdd51e2a48a4a4d7ab147e0f17d",
        hydrate_with_parsing=True,
    )
