from msgspec import Meta, Struct
from typing_extensions import Annotated


class AuthParameters(Struct):
    api_secret: Annotated[str, Meta(description="API Key used to access HrFlow.ai API")]
    api_user: Annotated[
        str, Meta(description="User email used to access HrFlow.ai API")
    ]
