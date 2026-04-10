import requests

from hrflow_connectors.v1.connectors.boondmanager.utils.jwt import auth_headers

BOONDMANAGER_BASE_URL = "https://ui.boondmanager.com/api"
REQUEST_TIMEOUT = 30  # seconds
PAGE_SIZE = 100  # BoondManager supports up to 500; 100 is a safe performant default


def fetch_app_dictionary(
    user_token: str, client_token: str, client_key: str, language: str = "fr"
) -> dict:
    """Fetch the BoondManager application dictionary (reference data for IDs → labels)."""
    response = requests.get(
        url=f"{BOONDMANAGER_BASE_URL}/application/dictionary",
        headers=auth_headers(user_token, client_token, client_key),
        params={"language": language},
        timeout=REQUEST_TIMEOUT,
    )
    response.raise_for_status()
    return response.json()
