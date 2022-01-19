from typing import Optional, List, Any
from pydantic import BaseModel, Field

# Job Model
class Location(BaseModel):
    name: str


class Department(BaseModel):
    id: int
    name: str
    parent_id: Any
    child_ids: Optional[List[int]]


class Office(BaseModel):
    id: int
    name: str
    location: str
    parent_id: Optional[int]
    child_ids: Optional[List[int]]


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


class Application(BaseModel):
    job_id: int


class Recruiter(BaseModel):
    id: Optional[int]
    email: Optional[str]


class coordinator(BaseModel):
    id: Optional[int]
    email: Optional[str]


class GreenhouseProfileModel(BaseModel):
    first_name: str = Field(..., description="The candidate's first name")
    last_name: str = Field(..., description="The candidate's last name")
    company: Optional[str] = Field(..., description="The candidate's company'")
    title: Optional[str] = Field(..., description="The candidate's title'")
    phone_numbers: Optional[List[PhoneNumber]] = Field(
        ..., description="Array of phone numbers. Passing an empty array will clear all"
    )
    addresses: Optional[List[Address]] = Field(
        ..., description="Array of addresses, passing an empty array will clear all"
    )
    email_addresses: Optional[List[EmailAddress]] = Field(
        ..., description="Array of email addresses, passing an empty array will"
    )
    website_addresses: Optional[List[WebsiteAddress]] = Field(
        ...,
        description="Array of website addresses, passing an empty array will clear all",
    )
    social_media_addresses: Optional[List[SocialMediaAddress]] = Field(
        ...,
        description="Array of social media addresses. Passing an empty array will clear all",
    )
    educations: Optional[List[Education]] = Field(
        ..., description="Array of education records"
    )
    employments: Optional[List[Employment]] = Field(
        ..., description="Array of employment records"
    )
    tags: Optional[List[str]] = Field(
        ...,
        description="Array of tags as strings. Passing an empty array will clear all",
    )
    applications: List[Application] = Field(
        ...,
        description="An array of application objects `dict(job_id=int)`. at least one is required",
    )
    recruiter: Optional[Recruiter] = Field(
        ..., description="An object representing the candidate's recruiter"
    )
    coordinator: Optional[coordinator] = Field(
        ..., description="An object representing the candidate's coordinator"
    )
