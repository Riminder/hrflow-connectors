from typing import Any, Optional, Dict

from pydantic import BaseModel, Field


class TaleezJobModel(BaseModel):
    id: int
    dateCreation: int
    dateFirstPublish: int
    dateLastPublish: int
    label: str = Field(..., description="Job Title")
    profile: str = Field(
        ..., description="Job Profile Type, for example administration, HR..."
    )
    currentStatus: str = Field(
        ..., description="status of the JOb whether it is Published, Draft or suspended"
    )
    contract: str = Field(..., description="Job Employment Type")
    contractLength: int
    fullTime: bool
    workHours: int
    qualification: str
    remote: bool
    country: str
    city: str
    postalCode: Any
    lat: Any
    lng: Any
    recruiterId: int
    companyLabel: str = Field(..., description="Company Name")
    url: str
    urlApplying: str
    jobDescription: str
    profileDescription: str
    companyDescription: str


class TaleezCandidateModel(BaseModel):
    firstName: str
    lastName: str
    mail: str
    phone: str
    initialReferrer: Optional[str]
    lang: str
    recruiterId: int
    socialLinks: Optional[Dict[str, str]]
