from typing import List
from pydantic import BaseModel


class Location(BaseModel):
    country: str
    countryCode: str
    regionCode: str
    region: str
    city: str
    lat: int
    lng: int


class Web(BaseModel):
    skype: str
    linkedin: str
    facebook: str
    twitter: str
    website: str


class EducationItem(BaseModel):
    institution: str
    degree: str
    major: str
    current: bool
    location: str
    startDate: str
    endDate: str
    description: str


class ExperienceItem(BaseModel):
    title: str
    company: str
    current: bool
    startDate: str
    endDate: str
    location: str
    description: str


class SmartrecruitersProfileModel(BaseModel):
    firstName: str
    lastName: str
    email: str
    phoneNumber: str
    location: Location
    web: Web
    tags: List[str]
    education: List[EducationItem]
    experience: List[ExperienceItem]
