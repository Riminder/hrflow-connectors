from datetime import datetime
from typing import List, Dict, Optional
from pydantic import BaseModel

class ContractDuration(BaseModel):
    min: Optional[int]
    max: Optional[int]

class Salary(BaseModel):
    min: Optional[int]
    max: Optional[int]
    kind: Optional[str]
    rate_type: Optional[str]
    variable: Optional[str]
    currency: Optional[str]

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
    position: Dict[str, str]

class Manager(BaseModel):
    section_title: str
    section_body: str
    picture_url: Optional[str]
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
    hierarchy: List[Hierarchy]

class ReferentRecruiter(BaseModel):
    firstname: str
    lastname: str
    picture_url: Optional[str]

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
    skills: List[str]
    salary: Salary
    pictures: List[str]
    videos: List[str]
    internal_apply_url: Optional[str]
    apply_url: Optional[str]
    address: Address
    entity: Entity
    referent_recruiter: ReferentRecruiter
    brand: Brand
    custom_fields: List[CustomField]
    count_recruited: Optional[str]


class DigitalRecruitersCandidateProfile(BaseModel):
    gender: int
    firstName: str
    lastName: str
    email: str
    phoneNumber: Optional[str] = None
    job: Optional[str] = None
    addressStreet: Optional[str] = None
    addressZip: Optional[str] = None
    addressCity: Optional[str] = None

class DigitalRecruitersImportCandidateMessage(BaseModel):
    message: str

class DigitalRecruitersImportCandidateFile(BaseModel):
    content: str
    name: str

class DigitalRecruitersImportCandidateProfile(BaseModel):
    reference: str
    consent_date: str
    s_o: str
    locale: str
    ApplicationMessage: DigitalRecruitersImportCandidateMessage
    ApplicationProfile: DigitalRecruitersCandidateProfile
    file: Optional[DigitalRecruitersImportCandidateFile] = None
