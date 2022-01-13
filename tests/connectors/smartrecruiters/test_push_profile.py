import pytest

from hrflow_connectors import XSmartTokenAuth
from hrflow_connectors import SmartRecruiters
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(credentials):
    auth = XSmartTokenAuth(
        value=credentials["smartrecruiters"]["oauth2"]["X-SmartToken"]
    )
    return auth


def test_SmartRecruitersPushProfileAction(logger, auth, hrflow_client):
    profile = Profile(
        key="89ddf5f18768747011a06b8921607cb54a4274a5",
        source=Source(key="6d68a20b2dd7c2bfdcb232b9234c38eada0fdcb4"),
    )
    response = SmartRecruiters.push_profile(
        auth=auth,
        job_id="3696cad0-a9b0-4a40-9cd7-4cc5feb1a509",
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    assert response.get("status_code") == 201