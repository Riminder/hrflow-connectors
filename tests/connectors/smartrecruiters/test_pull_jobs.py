import pytest

from hrflow_connectors import XSmartTokenAuth
from hrflow_connectors import SmartRecruiters


@pytest.fixture
def auth(credentials):
    auth = XSmartTokenAuth(
        value=credentials["smartrecruiters"]["oauth2"]["X-SmartToken"]
    )
    return auth


def test_PullJobsAction(logger, auth, hrflow_client):
    SmartRecruiters.pull_jobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        limit=2,
        board_key="1bed06c0123081959f830a920b3113d2540a02f7",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=False,
        posting_status="PUBLIC",
    )
