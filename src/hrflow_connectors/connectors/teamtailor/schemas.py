from typing import Optional, List, Any
from pydantic import BaseModel, Field


class TeamtailorJobAttribute(BaseModel):
    title: str
    pitch: str
    body: str = Field(..., description="job description")
    created_at: str = Field(..., alias="created-at")
    updated_at: str = Field(..., alias="updated-at")
    status: str
    tags: List[str]
    remote_status: str = Field(..., alias="remote-status")
    currency: Optional[str]
    salary_time_unit: Optional[str] = Field(..., alias="salary-time-unit")
    min_salary: int = Field(..., alias="min-salary")
    max_salary: int = Field(..., alias="max-salary")
    employment_type: Optional[str] = Field(..., alias="employment-type")
    employment_level: Optional[str] = Field(..., alias="employment-level")
    internal: Optional[bool]
    start_date: Optional[str] = Field(..., alias="start-date")
    end_date: Optional[str] = Field(..., alias="end-date")


class TeamtailorCandidateAttribute(BaseModel):
    first_name: str = Field(..., alias="first-name")
    last_name: str = Field(..., alias="last_name")
    email: str
    phone: Optional[str]
    pitch: Optional[str] = Field(..., description="summary")
    resume: Optional[str] = Field(..., description="candidate profile resume")
    sourced: Optional[bool]
    tags: List
