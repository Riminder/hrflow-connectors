import typing as t
from time import time

import requests

from hrflow_connectors.connectors.ukgpro.schemas import UKGProAuthentication
from hrflow_connectors.connectors.ukgpro.utils.errors import UKGProAuthenticationError

_UKGPRO_AUTHENTICATION_FURL = (
    "https://{identity_server}/signin/oauth2/t/{tenant}/access_token"
)


def _auth_headers_and_token_limit_get(
    auth_data: UKGProAuthentication,
) -> t.Tuple[t.Dict, float]:
    """
    Acquire authentication token.

    Args:
      auth_data (UKGProAuthentication): All the data needed in order to authenticate

    Returns:
      Tuple of authentication headers and token timestamp limit
    """

    headers = {"Content-Type": "application/x-www-form-urlencoded"}
    data = dict(
        grant_type=auth_data.grant_type,
        scope=" ".join(auth_data.scope),
        client_id=auth_data.client_id,
        client_secret=auth_data.client_secret,
    )

    response = requests.post(
        _UKGPRO_AUTHENTICATION_FURL.format(
            identity_server=auth_data.identity_server, tenant=auth_data.tenant
        ),
        data=data,
        headers=headers,
    )

    if response.status_code == requests.codes.ok:
        token_data = response.json()
        token_limit = time() + token_data["expires_in"]
        headers = dict(Authorization=f"Bearer {token_data['token']}")
        return headers, token_limit
    else:
        raise UKGProAuthenticationError(response.text)
