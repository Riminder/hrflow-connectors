import typing as t
from time import time

import requests
from pydantic import Field

from hrflow_connectors.connectors.cornerstoneondemand.schemas import (
    CornerstoneOnDemandAuthentication,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.enums import (
    CornerstoneOnDemandEndpoint,
    CornerstoneOnDemandEnv,
    CornerstoneOnDemandGrantType,
    CornerstoneOnDemandScope,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.errors import (
    CornerstoneOnDemandAuthenticationError,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.tools import (
    _api_formattable_url_get,
)
from hrflow_connectors.core import FieldType


def _token_limit_and_headers_get(
    auth_data: CornerstoneOnDemandAuthentication,
    scopes: t.List[CornerstoneOnDemandScope],
) -> t.Tuple[float, t.Dict]:
    """
    Authenticate to Cornerstone OnDemand by OAuth2. More details on:
    https://csod.dev/guides/getting-started/authentication.html

    Args:
      auth_data (CornerstoneOnDemandAuthentication): Cornerstone OnDemand credentials
      scopes (list[CornerstoneOnDemandScope]): the scopes of the token

    Returns:
      The timestamp at which the token will be considered expired and the
      authentication headers.
    """

    headers = {"Content-Type": "application/json", "cache-control": "no-cache"}
    data = dict(
        clientId=auth_data.client_id,
        clientSecret=auth_data.client_secret,
        grantType=CornerstoneOnDemandGrantType.CLIENT_CREDENTIALS,
        scope=" ".join(scopes),
    )

    response = requests.post(
        _api_formattable_url_get(CornerstoneOnDemandEndpoint.AUTHENTICATION).format(
            corpname=auth_data.corpname, env=CornerstoneOnDemandEnv.PRODUCTION
        ),
        data=data,
        headers=headers,
    )

    if response.status_code != requests.codes.ok:
        raise CornerstoneOnDemandAuthenticationError(response.text)
    else:
        token_data = response.json()
        token_limit = time() + token_data["expires_in"]
        headers = {
            "cache-control": "no-cache",
            "Authorization": f"{token_data['token_type']} {token_data['access_token']}",
        }
        return token_limit, headers


def _auth_data_field_get() -> Field:
    return Field(
        ...,
        description=(
            "All the data needed in order to obtain an access token for"
            " Cornerstone OnDemand Recruiting API."
        ),
        field_type=FieldType.Auth,
        repr=False,
    )
