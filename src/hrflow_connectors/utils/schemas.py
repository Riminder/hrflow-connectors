from typing import Union, Dict, Any, Optional, List
from pydantic import BaseModel, Field


# Hrflow Job model


from typing import Union, Dict, Any, Optional, List
from pydantic import BaseModel, Field


# Hrflow Job model


class HrflowJobSection(BaseModel):
    name: Optional[str] = Field(
        None, description="Identification name of a Section of the Job. Example: culture"
    )
    title: Optional[str] = Field(
        None, description="Display Title of a Section. Example: Corporate Culture"
    )
    description: Optional[str] = Field(
        None, description="Text description of a Section: Example: Our values areNone"
    )


class HrflowJobLocation(BaseModel):
    text: str = Field(None, description="Location text address.")
    lat: Optional[float] = Field(
        None, description="Geocentric latitude of the Location."
    )
    lng: Optional[float] = Field(
        None, description="Geocentric longitude of the Location."
    )
    Fields: Optional[Dict[str, Any]] = Field(
        None, description="other location attributes like country, country_codeNoneetc"
    )


class HrflowJobSKill(BaseModel):
    name: str = Field(None, description="Identification name of the skill")
    type: str = Field(None, description="Type of the skill. hard or soft")
    value: Optional[str] = Field(None, description="Value associated to the skill")


class HrflowJobField(BaseModel):
    """
    HrflowJobField: Languages, Certifications, Courses, Tasks, Interests, Metadatas, Tags
    are arrays of objects following the JSON structure below
    """

    name: str = Field(None, description="Identification name of the Object")
    value: Optional[str] = Field(
        None, description="Value associated to the Object's name"
    )


class HrflowJobRangesFloat(BaseModel):
    """
    HrflowJobRangesFloat: Ranges of floats is an array of objects accessible via the field ranges_float
    and follows the structure below
    """

    name: Optional[str] = Field(
        None,
        description="Identification name of a Range of floats attached to the Job. Example: salary",
    )
    value_min: Optional[float] = Field(None, description="Min value. Example: 500.")
    value_max: Optional[float] = Field(None, description="Max value. Example: 100.")
    unit: Optional[str] = Field(None, description="Unit of the value. Example: euros.")


class HrflowJobRangesDate(BaseModel):
    """
    HrflowJobRangesDate: Ranges of dates is an array of objects accessible via the field ranges_date
    and follows the structure below
    """

    name: Optional[str] = Field(
        None,
        description="Identification name of a Range of dates attached to the Job. Example: availability.",
    )
    value_min: Optional[str] = Field(
        None, description="Min value in datetime ISO 8601, Example: 500."
    )
    value_max: Optional[str] = Field(
        None, description="Max value in datetime ISO 8601, Example: 1000"
    )


class HrflowJob(BaseModel):
    """ Hrflow Job Object model """

    key: Optional[str] = Field(None, description="Identification key of the Job.")
    reference: Optional[str] = Field(None, description="Custom identifier of the Job.")
    name: str = Field(..., description="Job title.")
    location: HrflowJobLocation = Field(None, description="Job location object.")
    sections: List[HrflowJobSection] = Field(None, description="Job custom sections.")
    url: Optional[str] = Field(None, description="Job post original URL.")
    summary: Optional[str] = Field(None, description="Brief summary of the Job.")
    archieved_at: Optional[str] = Field(
        None,
        description="type: datetime ISO8601, Archive date of the Job. The value is null for unarchived Jobs.",
    )
    updated_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Last update date of the Job."
    )
    created_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Creation date of the Job."
    )
    skills: Optional[List[HrflowJobSKill]] = Field(
        None, description="List of skills of the Job."
    )
    languages: Optional[List[HrflowJobField]] = Field(
        None, description="List of spoken languages of the Job"
    )
    certifications: Optional[List[HrflowJobField]] = Field(
        None, description="List of certifications of the Job."
    )
    courses: Optional[List[HrflowJobField]] = Field(
        None, description="List of courses of the Job"
    )
    tasks: Optional[List[HrflowJobField]] = Field(
        None, description="List of tasks of the Job"
    )
    tags: Optional[List[HrflowJobField]] = Field(
        None, description="List of tags of the Job"
    )
    metadatas: Optional[List[HrflowJobField]] = Field(
        None, description="List of metadatas of the Job"
    )
    ranges_float: Optional[List[HrflowJobRangesFloat]] = Field(
        None, description="List of ranges of floats"
    )
    ranges_date: Optional[List[HrflowJobRangesDate]] = Field(
        None, description="List of ranges of dates"
    )



# Hflow Profile model


class FieldLocation(BaseModel):
    """ Location for profile info, experience and education information"""

    text: Optional[str] = Field(None, description="Location text address.")
    lat: Optional[float] = Field(
        None, description="Geocentric latitude of the Location."
    )
    lng: Optional[float] = Field(
        None, description="Geocentric longitude of the Location."
    )
    Fields: Optional[Dict[str, Any]] = Field(
        None, description="other location attributes like country, country_codeNoneetc"
    )


class FieldSkill(BaseModel):
    name: str = Field(..., description="Identification name of the skill")
    type: str = Field(..., description="Type of the skill. hard or soft")
    value: Optional[str] = Field(None, description="Value associated to the skill")


class InfoUrls(BaseModel):
    """ Porfile urls """

    from_resume: Optional[List[str]]
    linkedin: Optional[str]
    twitter: Optional[str]
    facebook: Optional[str]
    github: Optional[str]


class HrflowProfileField(BaseModel):
    """
    HrflowJobField: Languages, Certifications, Courses, Tasks, Interests, Metadatas, Tags
    are arrays of objects following the JSON structure below
    """

    name: str = Field(..., description="Identification name of the Object")
    value: Optional[str] = Field(
        None, description="Value associated to the Object's name"
    )


class HrflowProfileInfo(BaseModel):
    """ Profile basic informations"""

    full_name: Optional[str]
    first_name: Optional[str]
    last_name: Optional[str]
    email: Optional[str]
    phone: Optional[str]
    date_birth: Optional[str] = Field(None, description="Profile date of birth")
    location: Optional[FieldLocation] = Field(
        None, description="Profile location object"
    )
    urls: Optional[InfoUrls] = Field(
        None, description="Profile social networks and URLs"
    )
    picture: Optional[str] = Field(None, description="Profile picture url")
    gender: Optional[str] = Field(None, description="Profile gender")
    summary: Optional[str] = Field(None, description="Profile summary text")


class Experience(BaseModel):
    key: Optional[str] = Field(None, description="Identification key of the Experience.")
    company: Optional[str] = Field(None, description="Company name of the Experience.")
    title: Optional[str] = Field(None, description="Title of the Experience.")
    description: Optional[str] = Field(
        None, description="Description of the Experience."
    )
    location: Optional[FieldLocation] = Field(
        None, description="Location object of the Experience."
    )
    date_start: Optional[str] = Field(
        None, description="Start date of the experience. type: ('datetime ISO 8601')"
    )
    date_end: Optional[str] = Field(
        None, description="End date of the experience. type: ('datetime ISO 8601')"
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Experience."
    )
    certifications: Optional[List[HrflowProfileField]]
    courses: Optional[List[HrflowProfileField]]
    tasks: Optional[List[HrflowProfileField]]


class Education(BaseModel):
    key: Optional[str] = Field(None, description="Identification key of the Education.")
    school: Optional[str] = Field(None, description="School name of the Education.")
    title: Optional[str] = Field(None, description="Title of the Education.")
    description: Optional[str] = Field(None, description="Description of the Education.")
    location: Optional[FieldLocation] = Field(
        None, description="Location object of the Education."
    )
    date_start: Optional[str] = Field(
        None, description="Start date of the Education. type: ('datetime ISO 8601')"
    )
    date_end: Optional[str] = Field(
        None, description="End date of the Education. type: ('datetime ISO 8601')"
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Education."
    )
    certifications: Optional[List[HrflowProfileField]]
    courses: Optional[List[HrflowProfileField]]
    tasks: Optional[List[HrflowProfileField]]


class HrflowProfile(BaseModel):
    """ Hrflow Profile object model"""

    key: Optional[str] = Field(None, description="Identification key of the Profile.")
    reference: Optional[str] = Field(None, description="Custom identifier of the Profile.")
    archieved_at: Optional[str] = Field(
        None,
        description="type: datetime ISO8601, Archive date of the Profile. The value is null for unarchived Profiles.",
    )
    updated_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Last update date of the Profile."
    )
    created_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Creation date of the Profile."
    )
    info: HrflowProfileInfo = Field(
        None, description="Object containing the Profile's info."
    )
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
    experiences: Optional[List[Experience]] = Field(
        None, description="List of experiences of the Profile."
    )
    educations: Optional[List[Education]] = Field(
        None, description="List of educations of the Profile."
    )
    attachments: List = Field(
        None, description="List of documents attached to the Profile."
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Profile."
    )
    languages: Optional[List[HrflowProfileField]] = Field(
        None, description="List of spoken languages of the profile"
    )
    certifications: Optional[List[HrflowProfileField]] = Field(
        None, description="List of certifications of the Profile."
    )
    courses: Optional[List[HrflowProfileField]] = Field(
        None, description="List of courses of the Profile."
    )
    tasks: Optional[List[HrflowProfileField]] = Field(
        None, description="List of tasks of the Profile."
    )
    interests: Optional[List[HrflowProfileField]] = Field(
        None, description="List of interests of the Profile."
    )
    labels: Optional[List[HrflowProfileField]] = Field(
        None, description="List of labels of the Profile."
    )
    tags: Optional[List[HrflowProfileField]] = Field(
        None, description="List of tags of the Profile."
    )
    metadatas: Optional[List[HrflowProfileField]] = Field(
        None, description="List of metadatas of the Profile."
    )
