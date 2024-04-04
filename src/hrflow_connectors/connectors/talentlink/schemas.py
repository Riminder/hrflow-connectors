import typing as t

from pydantic import BaseModel


class Reference(BaseModel):
    employeeCode: str
    company: str
    email: str
    referredFrom: str


class ProfileLocation(BaseModel):
    address1: str
    address2: str
    city: str
    zip: str
    regionName: str
    homePhone: str
    workPhone: str
    mobilePhone: str
    fax: str
    alternateEmail: str
    personalWebSite: str
    preferredComChannel: str
    countryName: str


class PersonalData(BaseModel):
    dateOfBirth: int
    numberOfChildren: int
    citizenship: str
    language: str
    maritalStatus: str


class User(BaseModel):
    id: int
    login: str
    fullName: str
    language: str


class Consent(BaseModel):
    consentId: dict
    consentType: str
    status: str
    comment: str


class Tag(BaseModel):
    id: int
    name: str
    icon: str
    category: dict


class Referrer(BaseModel):
    firstName: str
    lastName: str
    email: str
    employeeCode: str
    employeeCompany: str


class TalentLinkProfile(BaseModel):
    type: str
    contractor: bool
    initialType: str
    firstname: str
    lastname: str
    middlename: str
    email: str
    academicTitle: str
    srcChannelName: str
    origin: str
    creation: int
    update: int
    socialSecurityNumber: str
    anonymous: bool
    formOfAddress: str
    address: ProfileLocation
    reference: Reference
    personalData: PersonalData
    creationUser: User
    updateUser: User
    sourceChannelType: str
    status: str
    sourcingMedium: str
    uiLanguage: str
    dataPrivacyStatement: str
    expectedArchiving: int
    memo: str
    linkedInId: str
    partiallyDeleted: bool
    latitude: float
    longitude: float
    internalCandidateId: str
    position: dict
    candidateConsents: t.List[Consent]
    tags: t.List[Tag]
    openingId: int
    poolId: int
    campaignTypes: t.List[dict]
    referrer: Referrer

    class Config:
        extra = "allow"


class TalentLinkJob(BaseModel):
    """"""
