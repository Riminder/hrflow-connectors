import pytest
from hrflow_connectors.connectors.destinations.sapsuccessfactors import PushProfile
from hrflow_connectors.core.auth import XAPIKeyAuth
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
        key = "4905d02723c363cb2fc55e7083921a28bb3de94f",
        source = Source(key="130d125dae175cfe318d260194dd84a3ad04110a")
    )
    action = PushProfile(
        auth = auth,
        subdomain="sandbox.api.sap.com:443/successfactors",
        hrflow_client=hrflow_client("dev-demo"),
        profile=profile,
    )
    action.execute()