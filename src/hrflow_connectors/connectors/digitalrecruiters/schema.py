import typing as t
from datetime import datetime

from pydantic import BaseModel, HttpUrl


class ContractDuration(BaseModel):
    min: t.Optional[int]
    max: t.Optional[int]


class Salary(BaseModel):
    min: t.Optional[int]
    max: t.Optional[int]
    kind: t.Optional[str]
    rate_type: t.Optional[str]
    variable: t.Optional[str]
    currency: t.Optional[str]


class AddressParts(BaseModel):
    street: str
    zip: str
    city: str
    county: str
    state: str
    country: str


class Address(BaseModel):
    parts: AddressParts
    formatted: str
    position: t.Dict[str, str]


class Manager(BaseModel):
    section_title: str
    section_body: str
    picture_url: t.Optional[str]
    firstname: str
    lastname: str
    position: str


class Hierarchy(BaseModel):
    depth: int
    column_name: str
    public_name: str


class Entity(BaseModel):
    public_name: str
    internal_ref: str
    around: str
    address: Address
    manager: Manager
    hierarchy: t.List[Hierarchy]


class ReferentRecruiter(BaseModel):
    firstname: str
    lastname: str
    picture_url: t.Optional[str]


class Brand(BaseModel):
    name: str
    description: str
    logo: str
    favicon: str


class CustomField(BaseModel):
    hash: str
    name: str
    value: str


class DigitalRecruitersJob(BaseModel):
    locale: str
    reference: str
    published_at: str
    catch_phrase: str
    contract_type: str
    contract_duration: ContractDuration
    contract_work_period: str
    service: str
    experience_level: str
    education_level: str
    title: str
    description: str
    profile: str
    skills: t.List[str]
    salary: Salary
    pictures: t.List[str]
    videos: t.List[str]
    internal_apply_url: t.Optional[str]
    apply_url: t.Optional[str]
    address: Address
    entity: Entity
    referent_recruiter: ReferentRecruiter
    brand: Brand
    custom_fields: t.List[CustomField]
    count_recruited: t.Optional[str]


class DigitalRecruitersCandidateProfile(BaseModel):
    gender: int
    firstName: str
    lastName: str
    email: str
    phoneNumber: t.Optional[str] = None
    job: t.Optional[str] = None
    addressStreet: t.Optional[str] = None
    addressZip: t.Optional[str] = None
    addressCity: t.Optional[str] = None


class DigitalRecruitersImportCandidateMessage(BaseModel):
    message: str


class DigitalRecruitersImportCandidateFile(BaseModel):
    content: str
    name: str


class DigitalRecruitersWriteProfile(BaseModel):
    reference: str
    consent_date: str
    s_o: str
    locale: str
    ApplicationMessage: DigitalRecruitersImportCandidateMessage
    ApplicationProfile: DigitalRecruitersCandidateProfile
    file: t.Optional[DigitalRecruitersImportCandidateFile] = None


class Location(BaseModel):
    zip: str
    city: str
    county: t.Optional[str]
    state: t.Optional[str]
    country: str
    latitude: t.Optional[float]
    longitude: t.Optional[float]


class ContractItem(BaseModel):
    id: int
    name: str
    countryNodeIds: t.Optional[t.List[int]]


class JobReference(BaseModel):
    label: str
    hashId: str


class Privacy(BaseModel):
    status: str
    updatedAt: t.Optional[datetime]


class Avatar(BaseModel):
    url: HttpUrl


class CV(BaseModel):
    url: HttpUrl


class Resume(BaseModel):
    raw: bytes
    content_type: str


class DigitalRecruitersReadProfile(BaseModel):
    id: int
    firstName: str
    lastName: str
    createdAt: datetime
    jobTitle: str
    avatar: Avatar
    gender: str
    email: str
    location: Location
    contract: ContractItem
    status: str
    jobReference: JobReference
    privacy: Privacy
    cv: CV
    resume: t.Optional[Resume]
