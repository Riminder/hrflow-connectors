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
        key="89ddf5f18768747011a06b8921607cb54a4274a5",
        source=Source(key="6d68a20b2dd7c2bfdcb232b9234c38eada0fdcb4"),
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