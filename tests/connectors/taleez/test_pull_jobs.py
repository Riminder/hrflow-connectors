import pytest

from hrflow_connectors import XTaleezAuth
from hrflow_connectors import Taleez


@pytest.fixture
def auth(config):
    return XTaleezAuth(value=config.TALEEZ_TOKEN)


def test_PullJobsAction(logger, auth, hrflow_client):
    Taleez.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="a820ecb28fc23b8217276cf2352387feaa5d7249",
        hydrate_with_parsing=True,
    )
