from typing import Optional

from pydantic import BaseModel


class Salary(BaseModel):
    min: Optional[float] = None
    max: Optional[float] = None
    currency: str


class Branch(BaseModel):
    id: int
    created_at: str
    updated_at: str
    deleted: bool
    name: str
    state: str
    city: str
    country_code: str
    zip: str
    time_zone: str
    currency: str
    language: str
    main_office: bool
    date_format: str
    street: str


class Department(BaseModel):
    id: int
    created_at: str
    updated_at: str
    deleted: bool
    name: str


class JobPostings(BaseModel):
    id: int
    created_at: str
    updated_at: str
    deleted: bool
    title: str
    description: str
    status: str
    salary: Salary
    applicant_access_type: str
    remote: bool
    show_pursue_as_career: bool
    closing_date: Optional[str]
    experience: str
    type: str
    branch: Branch
    department: Department
