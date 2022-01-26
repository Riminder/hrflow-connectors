from pydantic import BaseModel
from typing import List, Optional


class RecruiteJobModel(BaseModel):
    created_at: str
    title: str
    remote: bool
    slug: str
    options_cv: str
    category_code: str
    requirements: str
    min_hours: int
    status: str
    options_cover_letter: str
    experience_code: str
    company_name: str
    careers_url: str
    postal_code: str
    max_hours: int
    description: str
    department: str
    country_code: str
    id: int
    country: str
    careers_apply_url: str
    employment_type_code: str
    education_code: str
    city: str
    location: str


class RecruiteeProfile(BaseModel):
    name: str
    emails: Optional[List[str]]
    phones: Optional[List[str]]
    social_links: Optional[List[str]]
    links: Optional[List[str]]
    cover_letter: Optional[str]


class RecruiteeCandidateModel(BaseModel):
    candidate: RecruiteeProfile
    offers: Optional[List[int]]
