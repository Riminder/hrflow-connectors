import typing as t

from pydantic import BaseModel, Field


# Common
class Location(BaseModel):
    text: t.Optional[str] = Field(None, description="Location text address.")
    lat: t.Optional[float] = Field(
        None, description="Geocentric latitude of the Location."
    )
    lng: t.Optional[float] = Field(
        None, description="Geocentric longitude of the Location."
    )
    _fields: t.Optional[t.Dict[str, t.Any]] = Field(
        None,
        alias="fields",
        description="other location attributes like country, country_codeNoneetc",
    )


class GeneralEntitySchema(BaseModel):
    name: str = Field(..., description="Identification name of the Object")
    value: t.Optional[str] = Field(
        None, description="Value associated to the Object's name"
    )


class Skill(BaseModel):
    name: str = Field(..., description="Identification name of the skill")
    type: t.Optional[str] = Field(None, description="Type of the skill. hard or soft")
    value: t.Optional[str] = Field(None, description="Value associated to the skill")


# Job
class Section(BaseModel):
    name: t.Optional[str] = Field(
        None,
        description="Identification name of a Section of the Job. Example: culture",
    )
    title: t.Optional[str] = Field(
        None, description="Display Title of a Section. Example: Corporate Culture"
    )
    description: t.Optional[str] = Field(
        None, description="Text description of a Section: Example: Our values areNone"
    )


class RangesFloat(BaseModel):
    name: t.Optional[str] = Field(
        None,
        description=(
            "Identification name of a Range of floats attached "
            "to the Job. Example: salary"
        ),
    )
    value_min: t.Optional[float] = Field(None, description="Min value. Example: 500.")
    value_max: t.Optional[float] = Field(None, description="Max value. Example: 100.")
    unit: t.Optional[str] = Field(
        None, description="Unit of the value. Example: euros."
    )


class RangesDate(BaseModel):
    name: t.Optional[str] = Field(
        None,
        description=(
            "Identification name of a Range of dates attached"
            " to the Job. Example: availability."
        ),
    )
    value_min: t.Optional[str] = Field(
        None, description="Min value in datetime ISO 8601, Example: 500."
    )
    value_max: t.Optional[str] = Field(
        None, description="Max value in datetime ISO 8601, Example: 1000"
    )


class HrFlowJob(BaseModel):
    key: t.Optional[str] = Field(None, description="Identification key of the Job.")
    reference: t.Optional[str] = Field(
        None, description="Custom identifier of the Job."
    )
    name: str = Field(..., description="Job title.")
    location: Location = Field(None, description="Job location object.")
    sections: t.List[Section] = Field(None, description="Job custom sections.")
    url: t.Optional[str] = Field(None, description="Job post original URL.")
    summary: t.Optional[str] = Field(None, description="Brief summary of the Job.")
    archieved_at: t.Optional[str] = Field(
        None,
        description=(
            "type: datetime ISO8601, Archive date of the Job. "
            "The value is null for unarchived Jobs."
        ),
    )
    updated_at: t.Optional[str] = Field(
        None, description="type: datetime ISO8601, Last update date of the Job."
    )
    created_at: t.Optional[str] = Field(
        None, description="type: datetime ISO8601, Creation date of the Job."
    )
    skills: t.Optional[t.List[Skill]] = Field(
        None, description="t.List of skills of the Job."
    )
    languages: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of spoken languages of the Job"
    )
    certifications: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of certifications of the Job."
    )
    courses: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of courses of the Job"
    )
    tasks: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of tasks of the Job"
    )
    tags: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of tags of the Job"
    )
    metadatas: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="t.List of metadatas of the Job"
    )
    ranges_float: t.Optional[t.List[RangesFloat]] = Field(
        None, description="t.List of ranges of floats"
    )
    ranges_date: t.Optional[t.List[RangesDate]] = Field(
        None, description="t.List of ranges of dates"
    )


# Profile
class InfoUrls(BaseModel):
    from_resume: t.Optional[t.List[str]]
    linkedin: t.Optional[str]
    twitter: t.Optional[str]
    facebook: t.Optional[str]
    github: t.Optional[str]


class ProfileInfo(BaseModel):
    full_name: t.Optional[str]
    first_name: t.Optional[str]
    last_name: t.Optional[str]
    email: t.Optional[str]
    phone: t.Optional[str]
    date_birth: t.Optional[str] = Field(None, description="Profile date of birth")
    location: t.Optional[Location] = Field(None, description="Profile location object")
    urls: t.Optional[InfoUrls] = Field(
        None, description="Profile social networks and URLs"
    )
    picture: t.Optional[str] = Field(None, description="Profile picture url")
    gender: t.Optional[str] = Field(None, description="Profile gender")
    summary: t.Optional[str] = Field(None, description="Profile summary text")


class Experience(BaseModel):
    key: t.Optional[str] = Field(
        None, description="Identification key of the Experience."
    )
    company: t.Optional[str] = Field(
        None, description="Company name of the Experience."
    )
    title: t.Optional[str] = Field(None, description="Title of the Experience.")
    description: t.Optional[str] = Field(
        None, description="Description of the Experience."
    )
    location: t.Optional[Location] = Field(
        None, description="Location object of the Experience."
    )
    date_start: t.Optional[str] = Field(
        None, description="Start date of the experience. type: ('datetime ISO 8601')"
    )
    date_end: t.Optional[str] = Field(
        None, description="End date of the experience. type: ('datetime ISO 8601')"
    )
    skills: t.Optional[t.List[Skill]] = Field(
        None, description="List of skills of the Experience."
    )
    certifications: t.Optional[t.List[GeneralEntitySchema]]
    courses: t.Optional[t.List[GeneralEntitySchema]]
    tasks: t.Optional[t.List[GeneralEntitySchema]]


class Education(BaseModel):
    key: t.Optional[str] = Field(
        None, description="Identification key of the Education."
    )
    school: t.Optional[str] = Field(None, description="School name of the Education.")
    title: t.Optional[str] = Field(None, description="Title of the Education.")
    description: t.Optional[str] = Field(
        None, description="Description of the Education."
    )
    location: t.Optional[Location] = Field(
        None, description="Location object of the Education."
    )
    date_start: t.Optional[str] = Field(
        None, description="Start date of the Education. type: ('datetime ISO 8601')"
    )
    date_end: t.Optional[str] = Field(
        None, description="End date of the Education. type: ('datetime ISO 8601')"
    )
    skills: t.Optional[t.List[Skill]] = Field(
        None, description="List of skills of the Education."
    )
    certifications: t.Optional[t.List[GeneralEntitySchema]]
    courses: t.Optional[t.List[GeneralEntitySchema]]
    tasks: t.Optional[t.List[GeneralEntitySchema]]


class HrFlowProfile(BaseModel):
    key: t.Optional[str] = Field(None, description="Identification key of the Profile.")
    reference: t.Optional[str] = Field(
        None, description="Custom identifier of the Profile."
    )
    archived_at: t.Optional[str] = Field(
        None,
        description=(
            "type: datetime ISO8601, Archive date of the Profile."
            " The value is null for unarchived Profiles."
        ),
    )
    updated_at: t.Optional[str] = Field(
        None, description="type: datetime ISO8601, Last update date of the Profile."
    )
    created_at: t.Optional[str] = Field(
        None, description="type: datetime ISO8601, Creation date of the Profile."
    )
    info: ProfileInfo = Field(None, description="Object containing the Profile's info.")
    text_language: str = Field(
        None, description="Code language of the Profile. type: string code ISO 639-1"
    )
    text: str = Field(None, description="Full text of the Profile..")
    experiences_duration: float = Field(
        None, description="Total number of years of experience."
    )
    educations_duration: float = Field(
        None, description="Total number of years of education."
    )
    experiences: t.Optional[t.List[Experience]] = Field(
        None, description="List of experiences of the Profile."
    )
    educations: t.Optional[t.List[Education]] = Field(
        None, description="List of educations of the Profile."
    )
    attachments: t.List = Field(
        None, description="List of documents attached to the Profile."
    )
    skills: t.Optional[t.List[Skill]] = Field(
        None, description="List of skills of the Profile."
    )
    languages: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of spoken languages of the profile"
    )
    certifications: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of certifications of the Profile."
    )
    courses: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of courses of the Profile."
    )
    tasks: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of tasks of the Profile."
    )
    interests: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of interests of the Profile."
    )
    labels: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of labels of the Profile."
    )
    tags: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of tags of the Profile."
    )
    metadatas: t.Optional[t.List[GeneralEntitySchema]] = Field(
        None, description="List of metadatas of the Profile."
    )


class ResumeToParse(BaseModel):
    raw: bytes
    content_type: str


class HrFlowProfileParsing(BaseModel):
    reference: t.Optional[str] = Field(
        ..., description="Custom identifier of the Profile."
    )
    created_at: str = Field(
        ..., description="type: datetime ISO8601, Creation date of the Profile."
    )
    resume: ResumeToParse
    tags: t.List[GeneralEntitySchema] = Field(
        ..., description="List of tags of the Profile."
    )
    metadatas: t.List[GeneralEntitySchema] = Field(
        ..., description="List of metadatas of the Profile."
    )
