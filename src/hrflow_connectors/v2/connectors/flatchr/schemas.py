from enum import Enum
from typing import Optional

from msgspec import Struct


class FlatchrProfile(Struct):
    applicant: str
    vacancy: str
    column: str
    vacancy_id: int
    external_id: str
    status: int
    score: int
    hired: bool
    firstname: str
    lastname: str
    email: str
    phone: str
    created_at: str
    source: str


class Experience(int, Enum):
    NONE = 1  # ( < 1 year )
    MEDIUM = 2  # ( between 1 and 3 years )
    SENIOR = 4  # ( between 3 and 5 years )
    EXECUTIVE = 6  # ( > 6 years )


class FlatchrVacancy(Struct):
    id: str
    vacancy_id: Optional[int]
    slug: Optional[str]
    reference: Optional[str]
    title: str
    description: str
    experience: Experience
    mission: str
    profile: str
    salary: Optional[int]
    salary_max: Optional[int]
    status: int
    language: str
    contract_type_id: int
    education_level_id: int
    activity_id: int
    channel_id: int
    metier_id: int
    company_id: int
    mensuality: Optional[str]
    apply_url: Optional[str]
    currency: Optional[str]
    created_at: Optional[str]
    updated_at: Optional[str]
    start_date: Optional[str]
    end_date: Optional[str]
    driver_license: Optional[bool]
    remote: Optional[str]
    handicap: Optional[bool]
    partial: Optional[bool]
    meta_title: Optional[str]
    meta_description: Optional[str]
    meta_tags: Optional[str]
    options: dict
    video_url: Optional[str]
    address: str
    show_address: Optional[bool]
    show_contract_date: Optional[bool]
    show_contract_type: Optional[bool]
    show_salary: Optional[bool]
    worker_status: Optional[str]
    skills: Optional[str]
    kanban: Optional[bool]
    slug_mail: Optional[str]
