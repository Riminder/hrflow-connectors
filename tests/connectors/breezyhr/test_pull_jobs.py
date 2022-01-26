import pytest

from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors import Breezyhr


@pytest.fixture
def auth(credentials):
    auth = OAuth2EmailPasswordBody(
        access_token_url = "https://api.breezy.hr/v3/signin",
        email = credentials["breezyhr"]["email"],
        password= credentials["breezyhr"]["password"]
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    Breezyhr.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="194d9440437c157275f33ec9eda80a0250872e54",
        hydrate_with_parsing=True,
        company_name="Hrflow.ai"
    )
