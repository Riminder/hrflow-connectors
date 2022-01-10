import pytest

from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.connectors.destinations.greenhouse import PushProfile
from hrflow_connectors.utils.hrflow import Profile, Source
import base64



@pytest.fixture
def auth(credentials):
    auth = AuthorizationAuth(
        name = 'Authorization',
        value="Basic {}".format(base64.b64encode(b'a486aed6d051ac860db70f8d142d9868-4').decode("utf-8"))
    )
    return auth


def test_PushProfile(logger, auth, hrflow_client):

    profile = Profile(
        key="89ddf5f18768747011a06b8921607cb54a4274a5",
        source=Source(key="6d68a20b2dd7c2bfdcb232b9234c38eada0fdcb4"),
    )
    action = PushProfile(
        on_behalf_of = "limam.vadhel@hrflow.ai",
        prospect = True,
        auth=auth,
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    response = action.execute()
    assert response.get("status_code") == 201