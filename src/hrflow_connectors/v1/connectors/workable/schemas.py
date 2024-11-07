from typing import Optional

from pydantic import BaseModel, Field


class Location(BaseModel):
    location_str: Optional[str]
    country: Optional[str]
    country_code: Optional[str]
    region: Optional[str]
    region_code: Optional[str]
    city: Optional[str]
    zip_code: Optional[str]
    telecommuting: Optional[bool]


class WorkableJobModel(BaseModel):
    id: Optional[str]
    title: str
    full_title: str
    shortcode: str
    code: Optional[str]
    state: Optional[str]
    department: Optional[str]
    url: Optional[str]
    application_url: Optional[str]
    shortlink: Optional[str]
    location: Location
    created_at: str
    description: Optional[str]
    requirements: Optional[str]
    benefit: Optional[str]
    employment_type: Optional[str]


class Profile(BaseModel):
    name: str
    summary: Optional[str]
    address: Optional[str]
    headline: Optional[str] = Field(
        None, description="One line description as provided by the candidate or you"
    )
    phone: Optional[str]
    email: str
    resume_url: Optional[str]


class WorkableCandidate(BaseModel):
    sourced: Optional[bool] = True
    candidate: Profile
