import pytest
from hrflow_connectors import SapSuccessfactors
from hrflow_connectors import XAPIKeyAuth


@pytest.fixture
def auth(credentials):
    auth = XAPIKeyAuth(
        name='APIKey',
        value=credentials["sapsuccessfactors"]["oauth2"]["APIKey"],
    )
    return auth

def test_PullJobs(logger,auth, hrflow_client):
    SapSuccessfactors.pull_jobs(
        auth = auth,
        api_server="sandbox.api.sap.com:443/successfactors",
        top=20,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="193bc9ff5d3076937d93950381783e108c957fb3",
        hydrate_with_parsing=True,
    )
