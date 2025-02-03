import typing as t

from msgspec import Struct


class Location(Struct):
    name: str
    country: str
    address: str
    postal_code: str
    city: str
    region: str


class HomerunObject(Struct):
    name: str


class HomerunJob(Struct):
    id: str
    application_form_url: str
    job_url: str
    share_image_url: str
    status: str
    title: str
    description: str
    page_content: str
    total_candidate_count: int
    type: str
    expires_at: str
    is_remote: bool
    location_type: str
    location: Location
    department: HomerunObject
    stages: t.List[HomerunObject]
    created_at: str
