from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class BoondManagerOpportunity(BaseModel):
    id: str = Field(..., Description="Unique identifier of the opportunity")
    title: Optional[str] = Field(None, Description="Title / name of the opportunity")
    creationDate: Optional[str] = Field(
        None, Description="ISO-8601 date when the opportunity was created"
    )
    updateDate: Optional[str] = Field(
        None, Description="ISO-8601 date of the last update"
    )
    description: Optional[str] = Field(
        None, Description="Full text description of the opportunity"
    )
    place: Optional[Any] = Field(
        None, Description="Location information for the opportunity"
    )
    expertiseArea: Optional[Any] = Field(
        None, Description="Primary expertise area identifier"
    )
    activityAreas: Optional[List[Any]] = Field(
        None,
        Description="List of activity area identifiers associated with the opportunity",
    )
    origin: Optional[Dict[str, Any]] = Field(
        None,
        Description="Origin source of the opportunity (e.g. direct, referral)",
    )
    duration: Optional[Any] = Field(
        None, Description="Expected duration of the mission"
    )
    numberOfActivePositionings: Optional[int] = Field(
        None,
        Description="Number of candidates currently positioned on this opportunity",
    )


class BoondManagerCandidate(BaseModel):
    id: str = Field(..., Description="Unique identifier of the candidate")
    firstName: Optional[str] = Field(None, Description="Candidate's first name")
    lastName: Optional[str] = Field(None, Description="Candidate's last name")
    email1: Optional[str] = Field(None, Description="Primary email address")
    phone1: Optional[str] = Field(None, Description="Primary phone number")
    address: Optional[str] = Field(None, Description="Street address")
    postcode: Optional[str] = Field(None, Description="Postal / ZIP code")
    town: Optional[str] = Field(None, Description="City or town")
    country: Optional[str] = Field(None, Description="Country code")
    title: Optional[str] = Field(
        None, Description="Professional headline or current job title"
    )
    skills: Optional[str] = Field(
        None, Description="Free-text list of the candidate's skills"
    )
    diplomas: Optional[List[str]] = Field(
        None,
        Description=(
            "List of diploma strings (e.g. 'Master Computer Science - MIT - 2018')"
        ),
    )
    languages: Optional[List[Dict[str, Any]]] = Field(
        None,
        Description="List of language objects with typeOf and level identifiers",
    )
    socialNetworks: Optional[List[Dict[str, Any]]] = Field(
        None,
        Description="List of social network links (e.g. LinkedIn, GitHub)",
    )
    references: Optional[List[Dict[str, Any]]] = Field(
        None,
        Description="List of professional experience / reference objects",
    )
    state: Optional[int] = Field(
        None, Description="Candidate state identifier (e.g. active, archived)"
    )
    availability: Optional[Any] = Field(
        None, Description="Availability status or date of the candidate"
    )
    globalEvaluation: Optional[Any] = Field(
        None, Description="Overall evaluation score assigned to the candidate"
    )
    expertiseAreas: Optional[List[Any]] = Field(
        None, Description="List of expertise area identifiers"
    )
    activityAreas: Optional[List[Any]] = Field(
        None, Description="List of activity area identifiers"
    )
    mobilityAreas: Optional[List[Any]] = Field(
        None, Description="List of geographic mobility area identifiers"
    )
    numberOfResumes: Optional[int] = Field(
        None,
        Description="Number of resume files attached to this candidate",
    )
    creationDate: Optional[str] = Field(
        None, Description="ISO-8601 date when the candidate record was created"
    )
    updateDate: Optional[str] = Field(
        None,
        Description="ISO-8601 date of the last update to the candidate record",
    )
