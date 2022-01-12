from typing import List, Optional

from pydantic import BaseModel


class RecruiteeProfile(BaseModel):
    name: str
    emails: Optional[List[str]]
    phones: Optional[List[str]]
    social_links: Optional[List[str]]
    links: Optional[List[str]]
    cover_letter: Optional[str]


class RecruiteeCandidate(BaseModel):
    candidate: RecruiteeProfile
    offers: Optional[List[int]]
