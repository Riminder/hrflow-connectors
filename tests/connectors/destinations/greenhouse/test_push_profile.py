import pytest

from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.connectors.destinations.greenhouse import PushProfile
from hrflow_connectors.utils.hrflow import Profile, Source
import base64



@pytest.fixture
def auth(credentials):
    auth = AuthorizationAuth(
        name = 'Authorization',
        value= credentials["greenhouse"]["oauth2"]["Authorization"]
    )
    
    return auth


def test_PushProfile(logger, auth, hrflow_client):

    profile = Profile(
        key="65903f6989594e72ccf64a10875127b1293fdce3",
        source=Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    action = PushProfile(
        job_id = ["4179714004"] ,
        on_behalf_of = "4249018004",
        prospect = True,
        auth=auth,
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    response = action.execute()
    assert response.get("status_code") == 201