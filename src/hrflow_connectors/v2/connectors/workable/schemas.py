from typing import Optional, Union

from msgspec import Meta, Struct
from typing_extensions import Annotated


class Location(Struct):
    location_str: Optional[str]
    country: Optional[str]
    country_code: Optional[str]
    region: Optional[str]
    region_code: Optional[str]
    city: Optional[str]
    zip_code: Optional[str]
    telecommuting: Optional[bool]


class WorkableJob(Struct):
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


class EducationEntry(Struct):
    school: str
    degree: Optional[str]
    field_of_study: Optional[str]
    start_date: Optional[str]
    end_date: Optional[str]


class ExperienceEntry(Struct):
    company: Optional[str]
    title: str
    summary: Optional[str]
    industry: Optional[str]
    start_date: Optional[str]
    end_date: Optional[str]
    current: Optional[bool]


class SocialProfile(Struct):
    type: str
    name: str
    username: str
    url: str


class WorkableCandidate(Struct):
    name: str
    firstname: str
    lastname: str
    email: str
    headline: Annotated[
        Optional[str],
        Meta(description="One line description as provided by the candidate or you"),
    ]
    summary: Annotated[
        Optional[str], Meta(description="The profile summary provided by the candidate")
    ]
    address: Optional[str]
    phone: Optional[str]
    cover_letter: Optional[str]
    education_entries: Optional[list[EducationEntry]]
    experience_entries: Optional[list[ExperienceEntry]]
    answers: Optional[list[dict]]
    skills: Optional[Union[str, list[str]]]
    tags: Optional[list[str]]
    disqualified: Optional[bool]
    disqualification_reason: Optional[str]
    disqualified_at: Optional[str]
    social_profiles: Optional[list[SocialProfile]]
    domain: Optional[str]
    resume_url: Optional[str]
    recruiter_key: Optional[str]
