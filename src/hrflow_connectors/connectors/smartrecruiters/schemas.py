import typing as t

from pydantic import BaseModel


# Job
class Department(BaseModel):
    id: str


class JobLocation(BaseModel):
    country: t.Optional[str]
    countryCode: t.Optional[str]
    regionCode: t.Optional[str]
    region: t.Optional[str]
    city: str
    address: t.Optional[str]
    longitude: t.Optional[str]
    latitude: t.Optional[str]
    remote: t.Optional[bool]
    manual: t.Optional[bool]


class Industry(BaseModel):
    id: str


class Function(BaseModel):
    id: str


class TypeOfEmployment(BaseModel):
    id: str


class ExperienceLevel(BaseModel):
    id: t.Optional[str]


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


class SmartRecruitersJob(BaseModel):
    title: str
    refNumber: str
    createdOn: str
    updatedOn: str
    department: t.Optional[Department]
    location: JobLocation
    status: t.Optional[str]
    postingStatus: t.Optional[str]
    targetHiringDate: t.Optional[str]
    industry: t.Optional[Industry]
    function: t.Optional[Function]
    typeOfEmployment: t.Optional[TypeOfEmployment]
    experienceLevel: t.Optional[ExperienceLevel]
    eeoCategory: t.Optional[EeoCategory]
    creator: t.Optional[Creator]
    compensation: t.Optional[Compensation]
    jobAd: JobAd


# Profile
class ProfileLocation(BaseModel):
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


class SmartRecruitersProfile(BaseModel):
    firstName: str
    lastName: str
    email: str
    phoneNumber: str
    location: ProfileLocation
    web: Web
    tags: t.List[str]
    education: t.List[EducationItem]
    experience: t.List[ExperienceItem]
