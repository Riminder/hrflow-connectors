from msgspec import Meta, Struct
from typing_extensions import Annotated

SECRET_SMART_TAG = "smart::tag::smart"


class AuthParameters(Struct):
    smart_tag: Annotated[str, Meta(description="Tag to access Smart Leads API")]
