from typing import List

from msgspec import Struct


class Identity(Struct):
    seekerRefCode: str
    textResumeID: str
    resumeModifiedDate: str
    md5EmailAddress: str
    emailAddress: str
    name: str


class WorkAuthorization(Struct):
    authorization: str
    country: str


class Location(Struct):
    city: str
    state: str
    postalCode: str
    country: str
    willRelocate: bool
    workAuthorizations: List[WorkAuthorization]


class MetaData(Struct):
    name: str
    matched: str


class Experience(Struct):
    title: MetaData
    company: MetaData
    start: str
    end: str


class Skill(MetaData):
    yearsOfExperience: int
    lastUsed: str


class Relevance(Struct):
    score: int
    experience: Experience
    skills: List[Skill]


class Board(Struct):
    id: int
    name: str


class SecurityClearance(Struct):
    country: str
    clearance: str


class Salary(Struct):
    min: str
    max: str
    period: str
    currency: str


class PhoneNumber(Struct):
    phoneNumberValue: str
    priority: str


class Education(Struct):
    schoolName: str
    degree: str
    majors: List[str]
    start: str
    end: str


class Document(Struct):
    fileName: str
    fileContentType: str
    file: str


class MonsterProfile(Struct):
    identity: Identity
    location: Location
    yearsOfExperience: int
    relevance: Relevance
    veteran: bool
    viewed: bool
    lastActive: str
    boards: List[Board]
    resumeTitle: str
    securityClearance: SecurityClearance
    source: str
    targetJobTitle: str
    desiredSalary: Salary
    phoneNumbers: List[PhoneNumber]
    willTravel: bool
    highestEducationDegree: str
    educationalHistory: List[Education]
    externalRequisitions: List[str]
    resumeModifiedDate: str
    resume: str
    resumeDocument: Document
