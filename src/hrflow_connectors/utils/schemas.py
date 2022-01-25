from typing import Union, Dict, Any, Optional, List
from pydantic import BaseModel, Field


class HrflowJobSection(BaseModel):
    name: str = Field(
        ..., description="Identification name of a Section of the Job. Example: culture"
    )
    title: str = Field(
        ..., description="Display Title of a Section. Example: Corporate Culture"
    )
    description: str = Field(
        ..., description="Text description of a Section: Example: Our values are..."
    )


class HrflowJobLocation(BaseModel):
    text: str = Field(..., description="Location text address.")
    lat: Optional[float] = Field(
        None, description="Geocentric latitude of the Location."
    )
    lng: Optional[float] = Field(
        None, description="Geocentric longitude of the Location."
    )
    Fields: Optional[Dict[str, Any]] = Field(
        ..., description="other location attributes like country, country_code...etc"
    )


class HrflowJobSKill(BaseModel):
    name: str = Field(..., description="Identification name of the skill")
    type: str = Field(..., description="Type of the skill. hard or soft")
    value: Optional[str] = Field(None, description="Value associated to the skill")


class HrflowJobField(BaseModel):
    """
    HrflowJobField: Languages, Certifications, Courses, Tasks, Interests, Metadatas, Tags
    are arrays of objects following the JSON structure below
    """

    name: str = Field(..., description="Identification name of the Object")
    value: Optional[str] = Field(
        ..., description="Value associated to the Object's name"
    )


class HrflowJobRangesFloat(BaseModel):
    """
    HrflowJobRangesFloat: Ranges of floats is an array of objects accessible via the field ranges_float
    and follows the structure below
    """

    name: str = Field(
        ...,
        description="Identification name of a Range of floats attached to the Job. Example: salary",
    )
    value_min: float = Field(..., description="Min value. Example: 500.")
    value_max: float = Field(..., description="Max value. Example: 100.")
    unit: str = Field(..., description="Unit of the value. Example: euros.")


class HrflowJobRangesDate(BaseModel):
    """
    HrflowJobRangesDate: Ranges of dates is an array of objects accessible via the field ranges_date
    and follows the structure below
    """

    name: str = Field(
        ...,
        description="Identification name of a Range of dates attached to the Job. Example: availability.",
    )
    value_min: str = Field(
        ..., description="Min value in datetime ISO 8601, Example: 500."
    )
    value_max: str = Field(
        ..., description="Max value in datetime ISO 8601, Example: 1000"
    )


class HrflowJob(BaseModel):
    """ The Hrflow Job Object """

    key: Optional[str] = Field(..., description="Identification key of the Job.")
    reference: str = Field(..., description="Custom identifier of the Job.")
    name: str = Field(..., description="Job title.")
    location: HrflowJobLocation = Field(..., description="Job location object.")
    sections: List[HrflowJobSection] = Field(..., description="Job custom sections.")
    url: Optional[str] = Field(..., description="Job post original URL.")
    summary: Optional[str] = Field(..., description="Brief summary of the Job.")
    archieved_at: Optional[str] = Field(
        ...,
        description="type: datetime ISO8601, Archive date of the Job. The value is null for unarchived Jobs.",
    )
    updated_at: Optional[str] = Field(
        ..., description="type: datetime ISO8601, Last update date of the Job."
    )
    created_at: Optional[str] = Field(
        ..., description="type: datetime ISO8601, Creation date of the Job."
    )
    skills: Optional[List[HrflowJobSKill]] = Field(
        ..., description="List of skills of the Job."
    )
    languages: Optional[List[HrflowJobField]] = Field(
        ..., description="List of spoken languages of the Job"
    )
    certifications: Optional[List[HrflowJobField]] = Field(
        ..., description="List of certifications of the Job."
    )
    courses: Optional[List[HrflowJobField]] = Field(
        ..., description="List of courses of the Job"
    )
    tasks: Optional[List[HrflowJobField]] = Field(
        ..., description="List of tasks of the Job"
    )
    tags: Optional[List[HrflowJobField]] = Field(
        ..., description="List of tags of the Job"
    )
    metadatas: Optional[List[HrflowJobField]] = Field(
        ..., description="List of metadatas of the Job"
    )
    ranges_float: Optional[List[HrflowJobRangesFloat]] = Field(
        ..., description="List of ranges of floats"
    )
    ranges_date: Optional[List[HrflowJobRangesDate]] = Field(
        ..., description="List of ranges of dates"
    )
