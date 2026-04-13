"""BoondManager JWT authentication helpers.

BoondManager uses HMAC-SHA256 signed JWTs for API authentication.
The token embeds the current Unix timestamp and must be regenerated
on every request (or batch of requests) to avoid 401 / token-expired errors.
"""

import base64
import hashlib
import hmac
import json
import time
import typing as t


def _base64url_encode(data: t.Union[dict, bytes, str]) -> str:
    if isinstance(data, dict):
        data = json.dumps(data, separators=(",", ":")).encode()
    elif isinstance(data, str):
        data = data.encode()
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def _hmac_sha256(message: str, secret: str) -> str:
    return _base64url_encode(
        hmac.new(secret.encode(), message.encode(), hashlib.sha256).digest()
    )


def build_jwt(user_token: str, client_token: str, client_key: str) -> str:
    """Build a fresh BoondManager JWT.

    The JWT embeds the current Unix timestamp and is therefore short-lived.
    Call this function immediately before each request (or batch of requests)
    to avoid 401 / token-expired errors.
    """
    header = {"alg": "HS256", "typ": "JWT"}
    payload = {
        "userToken": user_token,
        "clientToken": client_token,
        "time": int(time.time()),
        "mode": "normal",
    }
    header_enc = _base64url_encode(header)
    payload_enc = _base64url_encode(payload)
    signature = _hmac_sha256(f"{header_enc}.{payload_enc}", client_key)
    return f"{header_enc}.{payload_enc}.{signature}"


def auth_headers(user_token: str, client_token: str, client_key: str) -> dict:
    """Return the Authorization header dict for a BoondManager API request."""
    return {
        "X-Jwt-Client-Boondmanager": build_jwt(user_token, client_token, client_key)
    }
