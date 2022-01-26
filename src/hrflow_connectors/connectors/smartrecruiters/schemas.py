from typing import List, Optional

from pydantic import BaseModel, Field

# Job Model
class Department(BaseModel):
    id: Optional[str]


class Location(BaseModel):
    country: Optional[Optional[str]]
    countryCode: Optional[Optional[str]]
    regionCode: Optional[Optional[str]]
    region: Optional[Optional[str]]
    city: Optional[str]
    address: Optional[Optional[str]]
    longitude: Optional[Optional[str]]
    latitude: Optional[Optional[str]]
    remote: Optional[bool]
    manual: Optional[bool]


class Industry(BaseModel):
    id: Optional[str]


class Function(BaseModel):
    id: Optional[str]


class TypeOfEmployment(BaseModel):
    id: Optional[str]


class ExperienceLevel(BaseModel):
    id: Optional[Optional[str]]


class EeoCategory(BaseModel):
    id: Optional[str]


class Creator(BaseModel):
    firstName: Optional[str]
    lastName: Optional[str]


class Compensation(BaseModel):
    min: int
    max: int
    currency: Optional[str]


class CompanyDescription(BaseModel):
    title: Optional[str]
    text: Optional[str]


class JobDescription(BaseModel):
    title: Optional[str]
    text: Optional[str]


class Qualifications(BaseModel):
    title: Optional[str]
    text: Optional[str]


class AdditionalInformation(BaseModel):
    title: Optional[str]
    text: Optional[str]


class Sections(BaseModel):
    companyDescription: CompanyDescription
    jobDescription: JobDescription
    qualifications: Qualifications
    additionalInformation: AdditionalInformation


class JobAd(BaseModel):
    sections: Sections


class SmartRecruitersModel(BaseModel):
    title: str
    refNumber: Optional[str]
    createdOn: Optional[str]
    updatedOn: Optional[str]
    department: Optional[Department]
    location: Location
    status: Optional[Optional[str]]
    postingStatus: Optional[Optional[str]]
    targetHiringDate: Optional[Optional[str]]
    industry: Optional[Industry]
    function: Optional[Function]
    typeOfEmployment: Optional[TypeOfEmployment]
    experienceLevel: Optional[ExperienceLevel]
    eeoCategory: Optional[EeoCategory]
    creator: Optional[Creator]
    compensation: Optional[Compensation]
    jobAd: JobAd


# Profile Model


class Location(BaseModel):
    country: Optional[str] = Field("Undefined")
    countryCode: Optional[str] = Field("NO")
    regionCode: Optional[str] =  Field("Undefined")
    region: Optional[str] = Field("Undefined")
    city: Optional[str] = Field("Undefined")
    lat: Optional[int] = Field(0)
    lng: Optional[int] = Field(0)


class Web(BaseModel):
    skype: Optional[str] = Field("Undefined")
    linkedin: Optional[str] = Field("Undefined")
    facebook: Optional[str] = Field("Undefined")
    twitter: Optional[str] = Field("Undefined")
    website: Optional[str] = Field("Undefined")


class EducationItem(BaseModel):
    institution: Optional[str]
    degree: Optional[str]
    major: Optional[str]
    current: Optional[bool]
    location: Optional[str]
    startDate: Optional[str]
    endDate: Optional[str]
    description: Optional[str]


class ExperienceItem(BaseModel):
    title: Optional[str]
    company: Optional[str]
    current: Optional[bool]
    startDate: Optional[str]
    endDate: Optional[str]
    location: Optional[str]
    description: Optional[str]


class SmartrecruitersProfileModel(BaseModel):
    firstName: str
    lastName: str
    email: Optional[str]
    phoneNumber: Optional[str]
    location: Location
    web: Optional[Web]
    tags: List[str]
    education: List[EducationItem]
    experience: List[ExperienceItem]
