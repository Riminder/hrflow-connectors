import pytest

from hrflow_connectors import XTaleezAuth
from hrflow_connectors import Taleez


@pytest.fixture
def auth(credentials):
    auth = XTaleezAuth(
        name = 'X-taleez-api-secret',
        value=credentials["taleez"]["X-taleez-api-secret"]
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    Taleez.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="234e96cf22440fa88cae5bca854d674f77fa8f93",
        hydrate_with_parsing=True,
    )
