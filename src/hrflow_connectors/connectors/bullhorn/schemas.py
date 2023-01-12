from typing import Any, Optional

from pydantic import BaseModel, Field


class BullhornAddress(BaseModel):
    address1: Optional[str] = Field(None, Description="Adress of the profile")
    city: Optional[str] = Field(None, Description="City of the profile")
    state: Optional[str] = Field(None, Description="Country code of the profile")
    zip: Optional[str] = Field(None, Description="Postal code of the profile")


class BullhornProfile(BaseModel):
    id: Optional[str] = Field(None, Description="Unique identifier for this entity")
    address: Optional[BullhornAddress] = Field(None, Description="Candidate address")
    certifications: Any = Field(None, Description="Candidate’s certifications")
    name: Optional[str] = Field(
        None,
        Description=(
            "Candidate’s full name. If setting firstname or lastname, you must also set"
            " this field; it does not populate automatically"
        ),
    )
    firstName: Optional[str] = Field(None, Description="Candidate’s first name")
    lastName: Optional[str] = Field(None, Description="Name of the file")
    email: Optional[Optional[str]] = Field(
        None, Description="Candidate’s email address"
    )
    mobile: Optional[Optional[str]] = Field(
        None, Description="Candidate’s mobile (cell) telephone number"
    )
    dateOfBirth: Optional[int] = Field(None, Description="Candidate’s date of birth")
    experience: Optional[int] = Field(
        None, Description="Number of years of experience that the Candidate has"
    )
    skillSet: Optional[str] = Field(
        None, Description="Text description of Candidate’s skills"
    )


class BullhornAttachmentEnrichment(BaseModel):
    externalID: Optional[str] = Field(
        None, Description="External identifier for the file"
    )
    fileContent: Optional[str] = Field(
        None, Description="Base64-encoded Optional[str]ing of the file content"
    )
    fileExtension: Optional[Optional[str]] = Field(
        None, Description="Extension of the file. For example, .doc or .jpg"
    )
    fileType: Optional[str] = Field(None, Description="Always use the value “SAMPLE”")
    name: Optional[str] = Field(
        None,
        Description=(
            "File name. If a file extension is included as part of the name and the "
            "fileExtension field is not set, the file extension in the name is used."
        ),
    )
    contentType: Optional[str] = Field(
        None, Description="Type/subtype of the file content.type"
    )
    description: Optional[str] = Field(
        None, Description="Unique identifier for this entity"
    )
    type: Optional[str]


class BullhornCandidate(BaseModel):
    id: Optional[Optional[int]] = Field(
        None, Description="Unique identifier for this entity"
    )


class BullhornExperienceEnrichment(BaseModel):
    id: Optional[str] = Field(None, Description="Unique identifier for this entity")
    candidate: BullhornCandidate = Field(
        None, Description="Candidate for whom this person is a reference"
    )
    companyName: Optional[Optional[str]] = Field(
        None,
        Description=(
            "Name of the company where reference works, if it does not "
            "have a ClientCorporation record in Bullhorn"
        ),
    )
    title: Optional[Optional[str]] = Field(
        None, Description="Candidate’s job title in this position"
    )
    comments: Optional[Optional[str]] = Field(
        None, Description="Free-text comments on CandidateWorkHistory"
    )
    startDate: Optional[Optional[int]] = Field(
        None, Description="Date on which Candidate began working at this position"
    )
    endDate: Optional[Optional[int]] = Field(
        None, Description="Date on which job ended, if applicable"
    )


class BullhornEducationEnrichment(BaseModel):
    id: Optional[str] = Field(None, Description="Unique identifier for this entity")
    candidate: BullhornCandidate = Field(
        None, Description="Candidate for whom this person is a reference"
    )
    school: Optional[Optional[str]] = Field(
        None,
        Description="Name of the educational institute where this education took place",
    )
    degree: Optional[Optional[str]] = Field(
        None,
        Description=(
            "Indicates what educational degree the Candidate received; for "
            "example, B.A., M.A., Ph.D., and so forth"
        ),
    )
    comments: Optional[Optional[str]] = Field(
        None, Description="Free-text comments on this record"
    )
    city: Optional[Optional[str]] = Field(
        None, Description="Name of the city where the education took place"
    )
    startDate: Optional[Optional[int]] = Field(
        None, Description="Date when Candidate began study"
    )
    endDate: Optional[Optional[int]] = Field(
        None, Description="Date when Candidate finished this education"
    )


class BullhornJob(BaseModel):
    id: Optional[Optional[int]] = Field(
        None, Description="Unique identifier for this entity"
    )
