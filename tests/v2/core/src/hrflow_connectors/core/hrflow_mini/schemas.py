import typing as t

from msgspec import Meta, Struct
from typing_extensions import Annotated


class Location(Struct):
    city: Annotated[str, Meta(description="City")]


class HrFlowMiniJob(Struct, kw_only=True):
    key: Annotated[
        t.Optional[str], Meta(description="Identification key of the Job.")
    ] = None
    reference: Annotated[
        t.Optional[str], Meta(description="Custom identifier of the Job.")
    ] = None
    status: t.Literal["created", "updated", "archived"]
    name: Annotated[str, Meta(description="Job title.")]
    location: Annotated[Location, Meta(description="Job location object.")]
    remote: Annotated[bool, Meta(description="Remote allowed.")]


class HrFlowMiniApplication(Struct, kw_only=True):
    candidate_id: Annotated[int, Meta(description="Candidate id")]
    job_key: Annotated[str, Meta(description="Job key")]
    outcome: t.Literal["pending", "accepted", "rejected"]
