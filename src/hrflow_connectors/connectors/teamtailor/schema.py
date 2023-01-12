from typing import Any, List, Optional

from pydantic import BaseModel, Field


class TeamtailorJobAttribute(BaseModel):
    title: str
    pitch: Optional[str]
    body: str = Field(..., description="job description")
    created_at: str = Field(..., alias="created-at")
    updated_at: Optional[str] = Field(..., alias="updated-at")
    status: Optional[str]
    tags: List[str]
    remote_status: Optional[str] = Field(..., alias="remote-status")
    currency: Optional[str]
    salary_time_unit: Optional[str] = Field(None, alias="salary-time-unit")
    min_salary: Optional[int] = Field(..., alias="min-salary")
    max_salary: Optional[int] = Field(..., alias="max-salary")
    employment_type: Optional[str] = Field(None, alias="employment-type")
    employment_level: Optional[str] = Field(None, alias="employment-level")
    internal: Optional[bool]
    start_date: Optional[str] = Field(None, alias="start-date")
    end_date: Optional[str] = Field(None, alias="end-date")


class TeamtailorJob(BaseModel):
    attributes: TeamtailorJobAttribute
    links: Optional[dict]
    id: Any


class TeamtailorCandidateAttribute(BaseModel):
    first_name: str = Field(..., alias="first-name")
    last_name: str = Field(..., alias="last-name")
    email: str
    phone: Optional[str]
    pitch: Optional[str] = Field(None, description="summary")
    resume: Optional[str] = Field(None, description="candidate profile resume")
    sourced: Optional[bool]
    tags: Optional[List]
