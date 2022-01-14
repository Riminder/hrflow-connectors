import pytest

from hrflow_connectors import XTaleezAuth
from hrflow_connectors import Taleez
from hrflow_connectors.utils.hrflow import Profile, Source

@pytest.fixture
def auth(credentials):
    auth = XTaleezAuth(
        name = 'X-taleez-api-secret',
        value=credentials["taleez"]["X-taleez-api-secret"]
    )
    return auth



def test_PushProfile(logger, auth, hrflow_client):

    profile = Profile(
        key="65903f6989594e72ccf64a10875127b1293fdce3",
        source=Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    response = Taleez.push_profile(
        job_id = None,
        auth=auth,
        recruiter_id = 15886,
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    assert response.get("status_code") == 201