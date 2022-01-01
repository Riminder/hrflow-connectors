import pytest

from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.connectors.destinations.flatchr import EnrichProfile
from hrflow_connectors.utils.hrflow import Profile, Source


@pytest.fixture
def auth(credentials):
    auth = AuthorizationAuth(value=credentials["flatchr"]["x-api-key"])
    return auth


def test_EnrichProfile(logger, auth, hrflow_client):
    profile = Profile(
        key="5746beca5e941a5a55706efd9adfce31f59e6e2b",
        source=Source(key="d42eed17626b7ae3dc05efca363788caef91d44b"),
    )
    action = EnrichProfile(
        auth=auth,
        subdomain="api",
        hrflow_client=hrflow_client(),
        profile=profile,
        vacancy="k0M5O9ylKZnxbQBy",
        compagny="LEZBvp5b4LdMoVmg",
    )
    response = action.execute()
    assert response.get("status_code") == 201