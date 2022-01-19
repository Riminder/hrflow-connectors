import pytest

from hrflow_connectors import AuthorizationAuth
from hrflow_connectors import Recruitee
from hrflow_connectors.utils.hrflow import Profile, Source

@pytest.fixture
def auth(credentials):
    auth = AuthorizationAuth(
        name = 'Authorization',
        value= credentials["recruitee"]["oauth2"]["Bearer"]
    )
    
    return auth


def test_PushProfile(logger, auth, hrflow_client):

    profile = Profile(
        key="65903f6989594e72ccf64a10875127b1293fdce3",
        source=Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb"),
    )
    response = Recruitee.push_profile(
        company_id = "hrflowai",
        offer_id = None,
        auth=auth,
        hrflow_client=hrflow_client(),
        profile=profile,
    )
    assert response.get("status_code") == 201