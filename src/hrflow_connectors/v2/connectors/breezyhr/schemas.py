from typing import Any, Dict, List, Optional

from msgspec import Meta, Struct
from typing_extensions import Annotated


class Type(Struct):
    id: str
    name: str


class Experience(Struct):
    id: str
    name: str


class Country(Struct):
    name: str
    id: str


class Location(Struct):
    country: Country
    city: str
    is_remote: Optional[bool]
    name: str


class Category(Struct):
    id: str
    name: str


class ApplicationForm(Struct):
    name: str
    headline: str
    summary: str
    profile_photo: str
    address: str
    email_address: str
    phone_number: str
    resume: str
    work_history: str
    education: str
    cover_letter: str
    questionnaire_in_experience: bool


class BreezyJobModel(Struct):
    _id: Annotated[str, Meta(description="position id")]
    type: Annotated[Type, Meta(description="job type")]
    state: Annotated[
        str,
        Meta(
            description="state of the position posting, published or internal and so on"
        ),
    ]
    name: Annotated[str, Meta(description="job title")]
    friendly_id: Annotated[
        str,
        Meta(description="another id of the job which combines its title and its id"),
    ]
    experience: Optional[Experience]
    location: Location
    education: str
    department: str
    description: Annotated[str, Meta(description="Job category")]
    category: Category
    application_form: Annotated[
        Optional[ApplicationForm], Meta(description="job Application for")
    ]
    creator_id: Optional[str]
    creation_date: str
    updated_date: str
    all_users: List[str]
    all_admins: List[str]
    candidate_type: str
    tags: List
    org_type: str


class WorkHistoryItem(Struct):
    company_name: str
    title: str
    summary: str
    start_month: Optional[int]
    start_year: Optional[int]
    end_month: Optional[int]
    end_year: Optional[int]


class EducationItem(Struct):
    school_name: str
    field_of_study: str
    start_year: Optional[int]
    end_year: Optional[int]


class BreezyProfileModel(Struct):
    name: str
    email_address: str
    phone_number: str
    summary: str
    tags: Optional[List[str]]
    source: Optional[str]
    origin: Optional[str]
    address: str
    work_history: List[WorkHistoryItem]
    education: List[EducationItem]
    social_profiles: Optional[List[str]]
    custom_attributes: Optional[List[Dict[str, Any]]]
    cover_letter: Optional[str]
