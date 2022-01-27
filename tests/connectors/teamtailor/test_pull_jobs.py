import pytest

from hrflow_connectors import AuthorizationAuth
from hrflow_connectors import Teamtailor


@pytest.fixture
def auth(credentials):
    auth = AuthorizationAuth(
        value=credentials["teamtailor"]["Authorization"]
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    Teamtailor.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="3fbfb874dc01a22e29abe639193c2cc319074712",
        hydrate_with_parsing=True,
    )