import pytest
from hrflow_connectors import SapSuccessfactors
from hrflow_connectors import XAPIKeyAuth
from hrflow_connectors.utils.hrflow import Profile, Source



@pytest.fixture
def auth(credentials):
    auth = XAPIKeyAuth(
        name='APIKey',
        value=credentials["sapsuccessfactors"]["oauth2"]["APIKey"],
    )
    return auth

def test_PushProfile(logger,auth, hrflow_client):

    profile = Profile(
        key = "65903f6989594e72ccf64a10875127b1293fdce3",
        source = Source(key="762d2f25b855f7cfd13e5585ef727d8fb6e752cb")
    )
    response = SapSuccessfactors.push_profile(
        profile_already_exists='COE0019',
        auth = auth,
        api_server="sandbox.api.sap.com:443/successfactors",
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    assert response.get("status_code") == 201