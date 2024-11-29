import typing as t

from msgspec import Struct


# Job
class Department(Struct):
    id: str


class JobLocation(Struct):
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


class Industry(Struct):
    id: str


class Function(Struct):
    id: str


class TypeOfEmployment(Struct):
    id: str


class ExperienceLevel(Struct):
    id: t.Optional[str]


class EeoCategory(Struct):
    id: str


class Creator(Struct):
    firstName: str
    lastName: str


class Compensation(Struct):
    min: int
    max: int
    currency: str


class CompanyDescription(Struct):
    title: str
    text: str


class JobDescription(Struct):
    title: str
    text: str


class Qualifications(Struct):
    title: str
    text: str


class AdditionalInformation(Struct):
    title: str
    text: str


class Sections(Struct):
    companyDescription: CompanyDescription
    jobDescription: JobDescription
    qualifications: Qualifications
    additionalInformation: AdditionalInformation


class JobAd(Struct):
    sections: Sections


class SmartRecruitersJob(Struct):
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
class ProfileLocation(Struct):
    country: str
    countryCode: str
    regionCode: str
    region: str
    city: str
    lat: int
    lng: int


class Web(Struct):
    skype: str
    linkedin: str
    facebook: str
    twitter: str
    website: str


class EducationItem(Struct):
    institution: str
    degree: str
    major: str
    current: bool
    location: str
    startDate: str
    endDate: str
    description: str


class ExperienceItem(Struct):
    title: str
    company: str
    current: bool
    startDate: str
    endDate: str
    location: str
    description: str


class sourceDetails(Struct):
    sourceTypeId: str
    sourceSubTypeId: t.Optional[str]
    sourceId: str


class SmartRecruitersProfile(Struct):
    firstName: str
    lastName: str
    email: str
    phoneNumber: str
    location: ProfileLocation
    web: Web
    tags: t.List[str]
    education: t.List[EducationItem]
    experience: t.List[ExperienceItem]
    sourceDetails: t.Optional[sourceDetails]
