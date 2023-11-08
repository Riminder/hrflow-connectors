import typing as t

from pydantic import BaseModel, Field

from hrflow_connectors.core import FieldType, ParametersModel


class area(BaseModel):
    areaId: int
    name: str


class Location(BaseModel):
    locationId: int
    name: str
    area: area


class Status(BaseModel):
    statusId: int
    name: str
    active: bool
    default: bool


class Owner(BaseModel):
    userId: int
    firstName: str
    lastName: str
    position: str
    email: str
    phone: str
    mobile: str
    inactive: bool
    deleted: bool


class Company(BaseModel):
    companyId: int
    name: str
    status: Status
    owner: Owner


class Contact(BaseModel):
    contactId: int
    firstName: str
    lastName: str
    position: str
    salutation: str
    unsubscribed: bool
    email: str
    phone: str
    mobile: str
    inactive: bool
    status: Status
    owner: Owner


class SkillTags(BaseModel):
    matchAll: bool
    tags: t.List[str]


class CustomFieldJob(BaseModel):
    fieldId: int
    name: str
    type: str
    value: t.Optional[str]
    valueList: t.Optional[t.List[str]]


class WorkplaceAddress(BaseModel):
    addressId: str
    name: str
    street: t.List[str]
    city: str
    state: str
    postalCode: str
    country: str
    countryCode: str
    phone: str
    fax: str
    url: str


class Start(BaseModel):
    immediate: bool
    relative: dict
    date: str


class WorkShift(BaseModel):
    startTime: str
    endTime: str
    workDays: t.List[str]


class Duration(BaseModel):
    period: int
    unit: str


class Salary(BaseModel):
    ratePer: str
    rateLow: int
    rateHigh: int
    currency: str
    timePerWeek: int


class Fee(BaseModel):
    rateType: str
    rate: int
    estimatedTotal: int
    currency: str


class PartnerAction(BaseModel):
    actionId: str
    actionName: str
    reference: str
    stage: str
    status: str
    result: dict
    submittedAt: str
    submittedBy: Owner
    updatedAt: str
    updatedBy: Owner


class StatisticApplications(BaseModel):
    new: int
    active: int
    total: int


class JobadderJob(BaseModel):
    jobId: int
    jobTitle: str
    location: Location
    company: Company
    contact: Contact
    status: Status
    source: str
    userFavourite: bool
    jobDescription: str
    numberOfJobs: int
    workplaceAddress: WorkplaceAddress
    category: dict
    start: Start
    endDate: str
    workShift: WorkShift
    duration: Duration
    workType: dict
    jobType: str
    salary: Salary
    fee: Fee
    otherContacts: t.List[Contact]
    skillTags: SkillTags
    custom: t.List[CustomFieldJob]
    owner: Owner
    recruiters: t.List[Owner]
    partnerActions: t.List[PartnerAction]
    createdBy: Owner
    createdAt: str
    updatedBy: Owner
    updatedAt: str
    closedBy: Owner
    closedAt: str
    statistics: dict
    links: dict


class Address(BaseModel):
    street: t.List[str]
    city: str
    state: str
    postalCode: str
    country: str
    countryCode: str


class User(BaseModel):
    userId: int
    firstName: str
    lastName: str
    position: str
    email: str
    phone: str
    mobile: str
    inactive: bool
    deleted: bool


class Social(BaseModel):
    facebook: str
    twitter: str
    linkedin: str
    youtube: str
    other: str


class Rating(BaseModel):
    period: int
    unit: str


class WorkType(BaseModel):
    workTypeId: int
    name: str
    ratePer: str


class Salary(BaseModel):
    ratePer: str
    rateLow: int
    rateHigh: int
    currency: str


class CustomField(BaseModel):
    fieldId: int
    name: str
    type: str
    value: t.List[str]


class Availability(BaseModel):
    immediate: bool
    relative: Rating
    date: str


class Education(BaseModel):
    institution: str
    course: str
    date: str


class JobadderCandidate(BaseModel):
    candidateId: int
    firstName: str
    lastName: str
    email: str
    phone: str
    mobile: str
    contactMethod: str
    salutation: str
    unsubscribed: bool
    address: Address
    status: dict
    rating: str
    source: str
    seeking: str
    dateOfBirth: str
    emergencyContact: str
    emergencyPhone: str
    otherEmail: t.List[str]
    social: Social
    summary: str
    employment: dict
    availability: Availability
    education: t.List[Education]
    skillTags: t.List[str]
    custom: t.List[CustomField]
    recruiters: t.List[User]
    createdBy: User
    createdAt: str
    updatedBy: User
    updatedAt: str
    partnerActions: dict
    statistics: dict
    links: dict


class JobsAdditionalParams(ParametersModel):
    jobId: list[int] = Field(
        [],
        description="Array of integers for Job Id (optional)",
        field_type=FieldType.QueryParam,
    )
    jobTitle: str = Field(
        None, description="Job title (optional)", field_type=FieldType.QueryParam
    )
    companyId: list[int] = Field(
        [],
        description="Alias for company.companyId (optional)",
        field_type=FieldType.QueryParam,
    )
    contactId: list[int] = Field(
        [], description="Contact Id (optional)", field_type=FieldType.QueryParam
    )
    statusId: list[int] = Field(
        [], description="Job status ID (optional)", field_type=FieldType.QueryParam
    )
    active: bool = Field(
        None,
        description="Search for active/open jobs (optional)",
        field_type=FieldType.QueryParam,
    )
    userFavourite: bool = Field(
        None,
        description="Search for the user's favorite jobs (optional)",
        field_type=FieldType.QueryParam,
    )
    folderId: list[int] = Field(
        [],
        description="Search in specific folders (optional)",
        field_type=FieldType.QueryParam,
    )
    userId: list[int] = Field(
        [],
        description=(
            "User ID - search for jobs by owner or associated recruiter (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    ownerUserId: list[int] = Field(
        [],
        description="User ID - search for jobs by owner (optional)",
        field_type=FieldType.QueryParam,
    )
    recruiterUserId: list[int] = Field(
        [],
        description="User ID - search jobs by associated recruiters (optional)",
        field_type=FieldType.QueryParam,
    )
    createdBy: list[int] = Field(
        [],
        description=(
            "User ID - search for jobs created by the specified user(s) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    createdAt: list[str] = Field(
        [],
        description=(
            "Search for jobs created at a specific date and time (UTC assumed, ISO"
            " date-time) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    updatedBy: list[int] = Field(
        [],
        description=(
            "User ID - search for jobs last updated by the specified user(s) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    updatedAt: list[str] = Field(
        [],
        description=(
            "Search for jobs updated at a specific date and time (UTC assumed, ISO"
            " date-time) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    closedBy: list[int] = Field(
        [],
        description=(
            "User ID - search for jobs last closed by the specified user(s) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    closedAt: list[str] = Field(
        [],
        description=(
            "Search for jobs closed at a specific date and time (UTC assumed, ISO"
            " date-time) (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    sort: list[str] = Field(
        [],
        description=(
            "Sort the results by one or multiple fields. Prefix with '-' to sort"
            " descending (optional)"
        ),
        field_type=FieldType.QueryParam,
    )
    fields: list[str] = Field(
        [],
        description="Additional fields to include with the results (optional)",
        field_type=FieldType.QueryParam,
    )
    embed: list[str] = Field(
        [],
        description="Embed related resources (optional)",
        field_type=FieldType.QueryParam,
    )


class CandidatesAdditionalParams(ParametersModel):
    candidateId: t.Optional[t.List[int]] = Field(
        ..., description="Candidate Id", field_type=FieldType.QueryParam
    )
    name: str = Field(
        None, description="Candidate name", field_type=FieldType.QueryParam
    )
    email: str = Field(
        None, description="Candidate email", field_type=FieldType.QueryParam
    )
    phone: str = Field(
        None,
        description="Candidate phone or mobile number",
        field_type=FieldType.QueryParam,
    )
    currentPosition: str = Field(
        None, description="Current Position", field_type=FieldType.QueryParam
    )
    city: str = Field(None, description="City", field_type=FieldType.QueryParam)
    state: str = Field(None, description="State", field_type=FieldType.QueryParam)
    location: str = Field(
        None,
        description="Location (city and/or state)",
        field_type=FieldType.QueryParam,
    )
    dateOfBirth: str = Field(
        None,
        description="Candidate date of birth available (ISO full-date)",
        field_type=FieldType.QueryParam,
    )
    keywords: str = Field(
        None,
        description="Search for key-words within the latest candidate resume",
        field_type=FieldType.QueryParam,
    )
    partnerAction: dict = Field(
        None, description="Partner Action parameters", field_type=FieldType.QueryParam
    )
    statusId: t.Optional[t.List[int]] = Field(
        ..., description="Candidate status", field_type=FieldType.QueryParam
    )
    recruiterUserId: t.Optional[t.List[int]] = Field(
        ...,
        description="User Id - search candidates by associated recruiters",
        field_type=FieldType.QueryParam,
    )
    folderId: t.Optional[t.List[int]] = Field(
        ..., description="Search in specific folders", field_type=FieldType.QueryParam
    )
    createdAt: t.Optional[t.List[str]] = Field(
        ...,
        description=(
            "Search for candidates created at a specific date and time (UTC assumed,"
            " ISO date-time)"
        ),
        field_type=FieldType.QueryParam,
    )
    updatedAt: t.Optional[t.List[str]] = Field(
        ...,
        description=(
            "Search for candidates updated at a specific date and time (UTC assumed,"
            " ISO date-time)"
        ),
        field_type=FieldType.QueryParam,
    )
    sort: t.Optional[t.List[str]] = Field(
        ...,
        description=(
            "Sort the results by one or multiple fields, prefix with '-' to sort"
            " descending"
        ),
        field_type=FieldType.QueryParam,
    )
    fields: t.Optional[t.List[str]] = Field(
        ...,
        description="Additional fields to include with the results",
        field_type=FieldType.QueryParam,
    )
    embed: t.Optional[t.List[str]] = Field(
        ..., description="Embed related resources", field_type=FieldType.QueryParam
    )
