import pytest

from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors import BreezyHr


@pytest.fixture
def auth(credentials):
    auth = OAuth2EmailPasswordBody(
        access_token_url = "https://api.breezy.hr/v3/signin",
        email = 'limam.vadhel@hrflow.ai',
        password="LIMAMok"
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    BreezyHr.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="fa06643e19d811e8da472858c07f8bbbd954dfd0",
        hydrate_with_parsing=True,
        company_name="Hrflow.ai"
    )