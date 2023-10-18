from datetime import datetime
from pydantic import BaseModel, Field
import typing as t


class WorkdayDescriptorId(BaseModel):
    descriptor: t.Optional[str] = Field(None, description="A preview of the instance")
    id: t.Optional[str] = Field(None, description="Id of the instance")


class WorkdayId(BaseModel):
    id: str = Field(
        ...,
        regex="^(?:(?:[0-9a-f]{32})|(?:[0-9]+\$[0-9]+)|(\S+=\S+))$",
        description="wid / id / reference id",
    )


class WorkdayOptionalId(BaseModel):
    id: t.Optional[str] = Field(None, description="Id of the instance")


class WorkdayPhone(WorkdayDescriptorId):
    countryPhoneCode: t.Optional[WorkdayId] = Field(None, description="Phone ~country~")
    deviceType: t.Optional[WorkdayId] = Field(
        None, description="The type of phone number."
    )
    extension: t.Optional[str] = Field(None, description="The phone number extension.")
    phoneNumber: t.Optional[str] = Field(
        None, description="The full primary phone number of the person."
    )


class WorkdayName(WorkdayDescriptorId):
    secondaryLocal: t.Optional[str] = Field(
        None,
        description=(
            "The person's secondary family name in local script. Workday only tracks"
            " local names for countries where a non-Latin script is commonly used."
        ),
    )
    firstNameLocal: t.Optional[str] = Field(
        None,
        description=(
            "The person's given name in local script. Workday only tracks local names"
            " for countries where a non-Latin script is commonly used."
        ),
    )
    firstNameLocal2: t.Optional[str] = Field(
        None,
        description=(
            "The person's given name in second local script. Workday only tracks local"
            " names for countries where a non-Latin script is commonly used."
        ),
    )
    social: t.Optional[WorkdayDescriptorId] = Field(
        None, description="Returns the social suffix from the name."
    )
    salutation: t.Optional[WorkdayDescriptorId] = Field(
        None, description="Returns the salutation from the name."
    )
    fullName: str = Field(
        ...,
        description=(
            "The Full Name for a person, when it is provided. Workday only tracks Full"
            " Name for countries where the Full Name name component is used."
        ),
    )
    lastNameLocal2: t.Optional[str] = Field(
        None,
        description=(
            "The person's last name in second local script. Workday only tracks local"
            " names for countries where a non-Latin script is commonly used."
        ),
    )
    lastName: str = Field(..., description="The person's family name.")
    secondaryLastName: t.Optional[str] = Field(
        None, description="The secondary family name for a person."
    )
    middleNameLocal: t.Optional[str] = Field(
        None,
        description=(
            "The person's middle name in local script. Workday only tracks local names"
            " for countries where a non-Latin script is commonly used."
        ),
    )
    country: t.Optional[WorkdayDescriptorId] = Field(
        None, description="Returns the ~country~ from the name."
    )
    firstName: str = Field(..., description="The first or given name for a person.")
    lastNameLocal: t.Optional[str] = Field(
        None,
        description=(
            "The person's last name in local script. Workday only tracks local names"
            " for countries where a non-Latin script is commonly used."
        ),
    )
    middleName: t.Optional[str] = Field(None, description="The person's middle name.")
    tertiaryLastName: t.Optional[str] = Field(
        None, description="The person's tertiary last name."
    )
    hereditary: t.Optional[WorkdayDescriptorId] = Field(
        None, description="Returns the hereditary suffix from the name."
    )
    title: t.Optional[WorkdayDescriptorId] = Field(
        None, description="Returns the prefix from the name."
    )


class WorkdayCandidate(WorkdayDescriptorId):
    email: t.Optional[str] = Field(None, description="The candidate's email address.")
    phone: t.Optional[WorkdayPhone] = Field(
        None, description="The candidate's primary home phone number."
    )
    name: WorkdayName = Field(
        ..., description="The global name associated with the candidate."
    )


class WorkdayEducation(WorkdayOptionalId):
    schoolName: str = Field(
        ...,
        description="The name of the school the candidate attended or is attending.",
    )
    degree: t.Optional[WorkdayId] = Field(
        None,
        description=(
            "The degree for a candidate's job application at the associated educational"
            " institution."
        ),
    )
    firstYearAttended: datetime = Field(
        ...,
        description=(
            "The first year the candidate attended this educational institution."
        ),
    )
    fieldOfStudy: t.Optional[WorkdayId] = Field(
        None,
        description=(
            "The field of study for the associated candidate at this educational"
            " institution."
        ),
    )
    lastYearAttended: datetime = Field(
        ...,
        description=(
            "The last year the candidate attended this educational institution."
        ),
    )
    gradeAverage: t.Optional[str] = Field(
        None,
        description="The candidate's grade average at this educational institution.",
    )


class WorkdayExperience(WorkdayOptionalId):
    companyName: str = Field(
        ..., description="The company name the candidate entered in their job history."
    )
    location: str = Field(..., description="The location of this company.")
    description: t.Optional[str] = Field(
        None,
        description=(
            "Any responsibilities or accomplishments that the candidate gained at the"
            " associated company."
        ),
    )
    startYear: datetime = Field(
        ..., description="The year the candidate started employment at this company."
    )
    currentlyWorkHere: t.Optional[bool] = Field(
        None, description="If true, the candidate currently works at this company."
    )
    title: str = Field(
        ..., description="The business title for the candidate's work experience."
    )
    startMonth: int = Field(
        ..., description="The month the candidate started employment at this company."
    )
    endYear: datetime = Field(
        ..., description="The year the candidate ended employment at this company."
    )
    endMonth: int = Field(
        ..., description="The month the candidate ended employment at this company."
    )


class WorkdaySkill(WorkdayOptionalId):
    name: str = Field(..., description="The name of the candidate skill.")


class WorkdayAbility(WorkdayOptionalId):
    proficiency: t.Optional[WorkdayId] = Field(
        None,
        description="Returns the proficiency for a specific ability of a language.",
    )
    abilityType: t.Optional[WorkdayId] = Field(
        None, description="Returns the language ability type."
    )


class WorkdayLanguage(WorkdayOptionalId):
    language: t.Optional[WorkdayId] = Field(
        None, description="Returns the language for this Language Skill."
    )
    abilities: t.Optional[t.List[WorkdayAbility]] = Field(
        None, description="The abilities associated with this language skill."
    )
    native: t.Optional[bool] = Field(
        None, description="If true, this language skill is the native language."
    )


class WorkdayResumeAttachments(WorkdayDescriptorId):
    fileLength: t.Optional[int] = Field(
        None, description="The file length of the attachment.", max_length=255
    )
    contentType: t.Optional[WorkdayId] = Field(
        None, description="Content type of the attachment."
    )
    fileName: str = Field(
        ..., description="The file name of the attachment. At most 255 characters."
    )

class WorkdayProspect(BaseModel):
    candidate: WorkdayCandidate = Field(
        ...,
        description="The candidate profile associated with this ~Prospect~.",
    )
    href: t.Optional[str] = Field(None, description="A link to the instance")
    candidateTags: t.Optional[t.List[WorkdayDescriptorId]] = Field(
        None,
        description="The candidate tags associated with the candidate.",
    )
    skills: t.Optional[t.List[WorkdaySkill]] = Field(
        None,
        description="The skills associated with the candidate.",
    )
    educations: t.Optional[t.List[WorkdayEducation]] = Field(
        None,
        description="The educations associated with the candidate.",
    )
    experiences: t.Optional[t.List[WorkdayExperience]] = Field(
        None,
        description="The experiences associated with the candidate.",
    )
    languages: t.Optional[t.List[WorkdayLanguage]] = Field(
        None,
        description="The languages associated with the candidate.",
    )
    resume: t.Optional[WorkdayResumeAttachments] = Field(
        None,
        description="The resume file associated with the candidate.",
    )
