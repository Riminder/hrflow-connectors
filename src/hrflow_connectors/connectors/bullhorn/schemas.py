from typing import Any, Optional
from pydantic import BaseModel, Field


class BullhornAddress(BaseModel):
    address1: str = Field(..., Description="Adress of the profile")
    city: str = Field(..., Description="City of the profile")
    state: str = Field(..., Description="Country code of the profile")
    zip: str = Field(..., Description="Postal code of the profile")


class BullhornProfile(BaseModel):
    id: str = Field(..., Description="Unique identifier for this entity")
    address: Optional[BullhornAddress] = Field(..., Description="Candidate address")
    certifications: Any = Field(..., Description="Candidate’s certifications")
    name: str = Field(..., Description="Candidate’s full name. If setting firstname or lastname, you must also set "
                                       "this field; it does not populate automatically")
    firstName: str = Field(..., Description="Candidate’s first name")
    lastName: str = Field(..., Description="Name of the file")
    email: Optional[str] = Field(..., Description="Candidate’s email address")
    mobile: Optional[str] = Field(..., Description="Candidate’s mobile (cell) telephone number")
    dateOfBirth: int = Field(..., Description="Candidate’s date of birth")
    experience: int = Field(..., Description="Number of years of experience that the Candidate has")
    skillSet: str = Field(..., Description="Text description of Candidate’s skills")


class BullhornAttachmentEnrichment(BaseModel):
    externalID: str = Field(..., Description="External identifier for the file")
    fileContent: str = Field(..., Description="Base64-encoded string of the file content")
    fileExtension: Optional[str] = Field(..., Description="Extension of the file. For example, .doc or .jpg")
    fileType: str = Field(..., Description="Always use the value “SAMPLE”")
    name: str = Field(..., Description="File name. If a file extension is included as part of the name and the "
                                       "fileExtension field is not set, the file extension in the name is used.")
    contentType: str = Field(..., Description="Type/subtype of the file content.type")
    description: str = Field(..., Description="Unique identifier for this entity")
    type: str


class BullhornCandidate(BaseModel):
    id: Optional[int] = Field(..., Description="Unique identifier for this entity")


class BullhornExperienceEnrichment(BaseModel):
    id: str = Field(..., Description="Unique identifier for this entity")
    candidate: BullhornCandidate = Field(..., Description="Candidate for whom this person is a reference")
    companyName: Optional[str] = Field(..., Description="Name of the company where reference works, if it does not "
                                                        "have a ClientCorporation record in Bullhorn")
    title: Optional[str] = Field(..., Description="Candidate’s job title in this position")
    comments: Optional[str] = Field(..., Description="Free-text comments on CandidateWorkHistory")
    startDate: Optional[int] = Field(..., Description="Date on which Candidate began working at this position")
    endDate: Optional[int] = Field(..., Description="Date on which job ended, if applicable")


class BullhornEducationEnrichment(BaseModel):
    id: str = Field(..., Description="Unique identifier for this entity")
    candidate: BullhornCandidate = Field(..., Description="Candidate for whom this person is a reference")
    school: Optional[str] = Field(..., Description="Name of the educational institute where this education took place")
    degree: Optional[str] = Field(..., Description="Indicates what educational degree the Candidate received; for "
                                                   "example, B.A., M.A., Ph.D., and so forth")
    comments: Optional[str] = Field(..., Description="Free-text comments on this record")
    city: Optional[str] = Field(..., Description="Name of the city where the education took place")
    startDate: Optional[int] = Field(..., Description="Date when Candidate began study")
    endDate: Optional[int] = Field(..., Description="Date when Candidate finished this education")
