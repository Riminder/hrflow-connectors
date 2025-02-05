from enum import Enum
from typing import List, Optional

from msgspec import Struct


class Kind(str, Enum):
    JOB = "job"
    TALENT_POOL = "talent_pool"


class RecruiteeProfile(Struct):
    name: str
    remote_cv_url: Optional[str]
    emails: Optional[List[str]]
    phones: Optional[List[str]]
    social_links: Optional[List[str]]
    links: Optional[List[str]]
    cover_letter: Optional[str]
    sources: Optional[List[str]]


class RecruiteeJob(Struct):
    title: str
    department: Optional[str]
    kind: Optional[Kind]
    description: str
    requirements: str
    postal_code: str
    city: str
    state_code: str
    country_code: str
    remote: bool
