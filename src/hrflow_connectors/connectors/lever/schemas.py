from pydantic import BaseModel, Field
import typing as t
from typing import List, Dict


class LeverJobContent(BaseModel):
    description: str
    descriptionHtml: str
    lists: List[Dict[str, str]]
    closing: str
    closingHtml: str

class LeverJobCategories(BaseModel):
    commitment: str
    department: str
    level: str
    location: str
    team: str

class LeverJobUrls(BaseModel):
    list: str
    show: str
    apply: str

class LeverJobSalaryRange(BaseModel):
    min: int
    max: int
    currency: str
    interval: str

class LeverJob(BaseModel):
    id: str
    text: str
    state: str
    distributionChannels: List[str]
    user: str
    owner: str
    hiringManager: str
    categories: LeverJobCategories
    tags: List[str]
    content: LeverJobContent
    country: str
    followers: List[str]
    reqCode: str
    requisitionCodes: List[str]
    urls: LeverJobUrls
    confidentiality: str
    createdAt: int
    updatedAt: int
    workplaceType: str
    salaryRange: LeverJobSalaryRange





class LeverProfile(BaseModel):
    id: str
    name: t.Dict[str, str]
    email: t.Optional[str]
    phone: t.Optional[str]
    createdAt: str
    updatedAt: str

    
