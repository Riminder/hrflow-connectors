from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class Type(BaseModel):
    id: str
    name: str


class Experience(BaseModel):
    id: str
    name: str


class Country(BaseModel):
    name: str
    id: str


class Location(BaseModel):
    country: Country
    city: str
    is_remote: Optional[bool]
    name: str


class Category(BaseModel):
    id: str
    name: str


class ApplicationForm(BaseModel):
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


class BreezyJobModel(BaseModel):
    _id: str = Field(..., description="position id")
    type: Type = Field(..., description="job type")
    state: str = Field(
        ...,
        description="state of the position posting, published or internal and so on",
    )
    name: str = Field(..., description="job title")
    friendly_id: str = Field(
        ..., description="another id of the job which combines its title and its id"
    )
    experience: Optional[Experience]
    location: Location
    education: str
    department: str
    description: str = Field(..., description="Job category")
    category: Category
    application_form: Optional[ApplicationForm] = Field(
        ..., description="job Application for"
    )
    creator_id: Optional[str]
    creation_date: str
    updated_date: str
    all_users: List[str]
    all_admins: List[str]
    candidate_type: str
    tags: List
    org_type: str


class WorkHistoryItem(BaseModel):
    company_name: str
    title: str
    summary: str
    start_month: Optional[int]
    start_year: Optional[int]
    end_month: Optional[int]
    end_year: Optional[int]


class EducationItem(BaseModel):
    school_name: str
    field_of_study: str
    start_year: Optional[int]
    end_year: Optional[int]


class BreezyProfileModel(BaseModel):
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
