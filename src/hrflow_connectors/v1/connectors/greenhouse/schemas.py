from typing import Any, List, Optional

from pydantic import BaseModel, Field


# Job Model
class Location(BaseModel):
    name: str


class Department(BaseModel):
    id: int
    name: str
    parent_id: Any
    child_ids: Optional[List[int]]
    external_id: int


class Office(BaseModel):
    id: int
    name: str
    location: str
    parent_id: Optional[int]
    child_ids: Optional[List[int]]


class Salary_range(BaseModel):
    min_value: int
    max_value: int
    unit: str


class Custom_fields(BaseModel):
    employment_type: str
    maximum_budget: str
    salary_range: Salary_range


class Employement_type(BaseModel):
    name: str
    type: str
    value: str


class Budget(BaseModel):
    name: str
    type: str
    value: str


class Keyed_custom_fields(BaseModel):
    employement_type = Employement_type
    budget: Budget
    salary_range: Salary_range


class Team_model(BaseModel):
    id: int
    first_name: str
    last_name: str
    name: str
    employee_id: str
    responsible: Optional[bool]


class Hiring_team(BaseModel):
    hiring_managers: List[Team_model]
    recruiters: List[Team_model]
    coordinators: List[Team_model]
    sourcers: List[Team_model]


class Close_reason(BaseModel):
    id: int
    name: str


class Opening(BaseModel):
    id: int
    opening_id: str
    status: str
    opened_at: str
    closed_at: str
    application_id: int
    close_reason: Close_reason


class GreenhouseJobModel(BaseModel):
    id: int
    internal_job_id: int
    title: str
    updated_at: Optional[str]
    requisition_id: Optional[str]
    location: Location
    absolute_url: str
    metadata: Any
    content: str
    departments: List[Department]
    offices: List[Office]


# Profile Model
class PhoneNumber(BaseModel):
    value: str
    type: str


class Address(BaseModel):
    value: str
    type: str


class EmailAddress(BaseModel):
    value: str
    type: str


class WebsiteAddress(BaseModel):
    value: str
    type: str


class SocialMediaAddress(BaseModel):
    value: str


class Education(BaseModel):
    school_id: int
    discipline_id: int
    degree_id: int
    start_date: str
    end_date: str


class Employment(BaseModel):
    company_name: str
    title: str
    start_date: str
    end_date: str


class Recruiter(BaseModel):
    id: Optional[int]
    email: Optional[str]


class coordinator(BaseModel):
    id: int
    email: str


class GreenhouseProfileModel(BaseModel):
    first_name: str = Field(..., description="The candidate's first name")
    last_name: str = Field(..., description="The candidate's last name")
    company: Optional[str] = Field(None, description="The candidate's company'")
    title: Optional[str] = Field(None, description="The candidate's title'")
    phone_numbers: Optional[List[PhoneNumber]] = Field(
        None,
        description="Array of phone numbers. Passing an empty array will clear all",
    )
    addresses: Optional[List[Address]] = Field(
        None, description="Array of addresses, passing an empty array will clear all"
    )
    email_addresses: Optional[List[EmailAddress]] = Field(
        None, description="Array of email addresses, passing an empty array will"
    )
    website_addresses: Optional[List[WebsiteAddress]] = Field(
        None,
        description="Array of website addresses, passing an empty array will clear all",
    )
    social_media_addresses: Optional[List[SocialMediaAddress]] = Field(
        None,
        description=(
            "Array of social media addresses. Passing an empty array will clear all"
        ),
    )
    educations: Optional[List[Education]] = Field(
        None, description="Array of education records"
    )
    employments: Optional[List[Employment]] = Field(
        None, description="Array of employment records"
    )
    tags: Optional[List[str]] = Field(
        None,
        description="Array of tags as strings. Passing an empty array will clear all",
    )
    applications: List[int] = Field(
        ...,
        description=(
            "An array of application objects `dict(job_id=int)`. at least one is"
            " required"
        ),
    )
    recruiter: Optional[Recruiter] = Field(
        None, description="An object representing the candidate's recruiter"
    )
    coordinator: Optional[coordinator]
