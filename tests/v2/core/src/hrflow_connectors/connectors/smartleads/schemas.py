import typing as t

from msgspec import Struct


class Lead(Struct):
    id: int
    category: str
    designation: str
    city: str
    status: t.Literal["created", "updated", "archived"]
    remote_allowed: bool
    candidate_count: int = 0


class Candidate(Struct):
    id: int
    first_name: str
    last_name: str
    status: t.Literal["created", "updated", "archived"]
    age: int
    has_driving_license: bool
