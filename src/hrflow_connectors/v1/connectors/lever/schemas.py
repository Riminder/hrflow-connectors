import typing as t

from pydantic import BaseModel


class LeverJobContent(BaseModel):
    description: str
    descriptionHtml: str
    lists: t.List[t.Dict[str, str]]
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
    distributionChannels: t.List[str]
    user: str
    owner: str
    hiringManager: str
    categories: LeverJobCategories
    tags: t.List[str]
    content: LeverJobContent
    country: str
    followers: t.List[str]
    reqCode: str
    requisitionCodes: t.List[str]
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
