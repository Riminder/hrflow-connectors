from msgspec import Meta, Struct
from typing_extensions import Annotated

SECRET_API_KEY = "hrflow::hrflower::hrflow"


class AuthParameters(Struct):
    api_key: Annotated[str, Meta(description="API Key")]
