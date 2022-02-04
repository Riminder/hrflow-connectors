from hrflow_connectors import Workable
from hrflow_connectors import AuthorizationAuth
import pytest

@pytest.fixture
def auth(config):
    return AuthorizationAuth(value=config.WORKABLE_TOKEN)

def test_PullJobsAcrion(logger,auth, hrflow_client):
    Workable.pull_jobs(
        subdomain="arabeg",
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="0d9a50e90e529e43394cf84b2f0666432551980b",
        hydrate_with_parsing=True,
    )