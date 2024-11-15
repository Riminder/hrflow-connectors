from typing import Any, Optional

from msgspec import Meta, Struct
from typing_extensions import Annotated


class BullhornAddress(Struct):
    address1: Annotated[Optional[str], Meta(description="Adress of the profile")] = None
    city: Annotated[Optional[str], Meta(description="City of the profile")] = None
    state: Annotated[Optional[str], Meta(description="Country code of the profile")] = (
        None
    )
    zip: Annotated[Optional[str], Meta(description="Postal code of the profile")] = None


class BullhornProfile(Struct):
    id: Annotated[
        Optional[str], Meta(description="Unique identifier for this entity")
    ] = None
    address: Annotated[
        Optional[BullhornAddress], Meta(description="Candidate address")
    ] = None
    certifications: Annotated[Any, Meta(description="Candidate’s certifications")] = (
        None
    )
    name: Annotated[
        Optional[str],
        Meta(
            description=(
                "Candidate’s full name. If setting firstname or lastname, you must also"
                " set this field; it does not populate automatically"
            ),
        ),
    ] = None
    firstName: Annotated[Optional[str], Meta(description="Candidate’s first name")] = (
        None
    )
    lastName: Annotated[Optional[str], Meta(description="Name of the file")] = None
    email: Annotated[
        Optional[Optional[str]], Meta(description="Candidate’s email address")
    ] = None
    mobile: Annotated[
        Optional[Optional[str]],
        Meta(description="Candidate’s mobile (cell) telephone number"),
    ] = None
    dateOfBirth: Annotated[
        Optional[int], Meta(description="Candidate’s date of birth")
    ] = None
    experience: Annotated[
        Optional[int],
        Meta(description="Number of years of experience that the Candidate has"),
    ] = None
    skillSet: Annotated[
        Optional[str], Meta(description="Text description of Candidate’s skills")
    ] = None


class BullhornAttachmentEnrichment(Struct, kw_only=True):
    externalID: Annotated[
        Optional[str], Meta(description="External identifier for the file")
    ] = None
    fileContent: Annotated[
        Optional[str],
        Meta(description="Base64-encoded Optional[str]ing of the file content"),
    ] = None
    fileExtension: Annotated[
        Optional[Optional[str]],
        Meta(description="Extension of the file. For example, .doc or .jpg"),
    ] = None
    fileType: Annotated[
        Optional[str], Meta(description="Always use the value “SAMPLE”")
    ] = None
    name: Annotated[
        Optional[str],
        Meta(
            description=(
                "File name. If a file extension is included as part of the name and the"
                " fileExtension field is not set, the file extension in the name is"
                " used."
            ),
        ),
    ] = None
    contentType: Annotated[
        Optional[str], Meta(description="Type/subtype of the file content.type")
    ] = None
    description: Annotated[
        Optional[str], Meta(description="Unique identifier for this entity")
    ] = None
    type: Optional[str]


class BullhornCandidate(Struct):
    id: Annotated[
        Optional[Optional[int]], Meta(description="Unique identifier for this entity")
    ] = None


class BullhornExperienceEnrichment(Struct, kw_only=True):
    id: Annotated[
        Optional[str], Meta(description="Unique identifier for this entity")
    ] = None
    candidate: Annotated[
        BullhornCandidate,
        Meta(description="Candidate for whom this person is a reference"),
    ]
    companyName: Annotated[
        Optional[Optional[str]],
        Meta(
            description=(
                "Name of the company where reference works, if it does not "
                "have a ClientCorporation record in Bullhorn"
            ),
        ),
    ] = None
    title: Annotated[
        Optional[Optional[str]],
        Meta(description="Candidate’s job title in this position"),
    ] = None
    comments: Annotated[
        Optional[Optional[str]],
        Meta(description="Free-text comments on CandidateWorkHistory"),
    ] = None
    startDate: Annotated[
        Optional[Optional[int]],
        Meta(description="Date on which Candidate began working at this position"),
    ] = None
    endDate: Annotated[
        Optional[Optional[int]],
        Meta(description="Date on which job ended, if applicable"),
    ] = None


class BullhornEducationEnrichment(Struct, kw_only=True):
    id: Annotated[
        Optional[str], Meta(description="Unique identifier for this entity")
    ] = None
    candidate: Annotated[
        BullhornCandidate,
        Meta(description="Candidate for whom this person is a reference"),
    ]
    school: Annotated[
        Optional[Optional[str]],
        Meta(
            description=(
                "Name of the educational institute where this education took place"
            )
        ),
    ] = None
    degree: Annotated[
        Optional[Optional[str]],
        Meta(
            description=(
                "Indicates what educational degree the Candidate received; for "
                "example, B.A., M.A., Ph.D., and so forth"
            ),
        ),
    ] = None
    comments: Annotated[
        Optional[Optional[str]], Meta(description="Free-text comments on this record")
    ] = None
    city: Annotated[
        Optional[Optional[str]],
        Meta(description="Name of the city where the education took place"),
    ] = None
    startDate: Annotated[
        Optional[Optional[int]], Meta(description="Date when Candidate began study")
    ] = None
    endDate: Annotated[
        Optional[Optional[int]],
        Meta(description="Date when Candidate finished this education"),
    ] = None


class BullhornJob(Struct):
    id: Annotated[
        Optional[Optional[int]], Meta(description="Unique identifier for this entity")
    ] = None
