from typing import List, Optional

from pydantic import BaseModel


class Department(BaseModel):
    id: str


class Location(BaseModel):
    country: Optional[str]
    countryCode: Optional[str]
    regionCode: Optional[str]
    region: Optional[str]
    city: str
    address: Optional[str]
    longitude: Optional[str]
    latitude: Optional[str]
    remote: Optional[bool]
    manual: Optional[bool]


class Industry(BaseModel):
    id: str

class Function(BaseModel):
    id: str


class TypeOfEmployment(BaseModel):
    id: str

class ExperienceLevel(BaseModel):
    id: Optional[str]

class EeoCategory(BaseModel):
    id: str


class Creator(BaseModel):
    firstName: str
    lastName: str


class Compensation(BaseModel):
    min: int
    max: int
    currency: str


class CompanyDescription(BaseModel):
    title: str
    text: str


class JobDescription(BaseModel):
    title: str
    text: str


class Qualifications(BaseModel):
    title: str
    text: str


class AdditionalInformation(BaseModel):
    title: str
    text: str

class Sections(BaseModel):
    companyDescription: CompanyDescription
    jobDescription: JobDescription
    qualifications: Qualifications
    additionalInformation: AdditionalInformation



class JobAd(BaseModel):
    sections: Sections


class SmartRecruitersModel(BaseModel):
    title: str
    refNumber: str
    createdOn: str
    updatedOn: str
    department: Optional[Department]
    location: Location
    status: Optional[str]
    postingStatus: Optional[str]
    targetHiringDate: Optional[str]
    industry: Optional[Industry]
    function: Optional[Function]
    typeOfEmployment: Optional[TypeOfEmployment]
    experienceLevel: Optional[ExperienceLevel]
    eeoCategory: Optional[EeoCategory]
    creator: Optional[Creator]
    compensation: Optional[Compensation]
    jobAd: JobAd
