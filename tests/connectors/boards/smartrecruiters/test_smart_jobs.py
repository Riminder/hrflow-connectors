import pytest

from hrflow_connectors.core.auth import XSmartTokenAuth
from hrflow_connectors.connectors.boards.smartrecruiters import SmartJobs


@pytest.fixture
def auth(credentials):
    auth = XSmartTokenAuth(
        value=credentials["smartrecruiters"]["oauth2"]["X-SmartToken"]
    )
    return auth


def test_SmartJobs(logger, auth, hrflow_client):
    action = SmartJobs(
        auth=auth,
        hrflow_client=hrflow_client("dev-demo"),
        limit=2,
        board_key="8ebdea98768dfc04d15f76afab70415ed280ea90",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=False,
        posting_status="PUBLIC",
    )
    action.execute()
