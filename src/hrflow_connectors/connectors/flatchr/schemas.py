from typing import List, Optional

from pydantic import BaseModel, Field


class FlatchrResume(BaseModel):
    fileName: Optional[str] = Field("resume.pdf", Description="Name of the file")
    contentType: Optional[str] = Field(
        "application/octet-stream", Description="Type of the content of the file"
    )
    data: Optional[str] = Field(None, Description="Content of the file")


class FlatchrCreationProfile(BaseModel):
    vacancy: Optional[str] = Field(None, Description="Unique identifier of the ad")
    firstname: Optional[str] = Field(None, Description="First name /[a-zA-Z]/")
    lastname: Optional[str] = Field(None, Description="Last name /[a-zA-Z]/")
    type: Optional[str] = Field("applicants", Description="'applicants' by default")
    resume: FlatchrResume = Field(
        None,
        Description=(
            "Type of format for the CV: document (if cv base64, default), "
            "json (if cv, hr-xml format)"
        ),
    )
    email: Optional[str] = Field(
        None,
        Description=(
            "Email will be used as a reference for the profile. The enrichment request "
            "aim the right profile to enrich with the email."
        ),
    )
    phone: Optional[str] = Field("XXXXX", Description="Phone")


class FlatchrAdressProfile(BaseModel):
    formattedAddress: Optional[Optional[str]] = Field(
        None, Description="Adress where the education were done"
    )


class FlatchrCommunicationProfile(BaseModel):
    address: List[FlatchrAdressProfile] = Field(
        None, Description="List of adresses where the education were done"
    )


class FlatchrInstitutionProfile(BaseModel):
    communication: FlatchrCommunicationProfile = Field(
        None, Description="List of adresses"
    )
    name: Optional[str] = Field(
        None, Description="Name of the institution in which the education was provided"
    )


class FlatchrEducationLevelCode(BaseModel):
    name: Optional[str] = Field(None, Description="Degree of the education")


class FlatchrEducationDegree(BaseModel):
    name: Optional[str] = Field(None, Description="Degree of the education")
    date: Optional[str] = Field(
        None, Description="Date on which the degree was obtained"
    )
    specializations: List = Field([], Description="Specializations of the degree")


class FlatchrEducationProfile(BaseModel):
    institution: FlatchrInstitutionProfile = Field(
        None, Description="Institution object"
    )
    educationLevelCodes: List[FlatchrEducationLevelCode] = Field(
        None, Description="List of all the degrees"
    )
    educationDegrees: List[FlatchrEducationDegree] = Field(
        None, Description="List of all the degrees"
    )
    end: Optional[str] = Field("XXXX", Description="Date on which the degree has ended")
    descriptions: List[Optional[str]] = Field(
        [None], Description="Description of the education"
    )


class FlatchrGeoLocation(BaseModel):
    latitude: Optional[float] = Field(
        None, Description="Latitude of the employment location"
    )
    longitude: Optional[float] = Field(
        None, Description="Longitude of the employment location"
    )


class FlatchrAddres1(BaseModel):
    countryCode: Optional[str] = Field(
        None, Description="Country code of the employment location"
    )
    city: Optional[str] = Field(None, Description="City of the employment location")
    postalCode: Optional[str] = Field(
        None, Description="Postalcode of the employment location"
    )
    geoLocation: FlatchrGeoLocation = Field(
        None, Description="GeoLocation object of the employment location"
    )
    formattedAddress: Optional[str] = Field(
        None, Description="Address of the employment location"
    )


class FlatchrCommunication1(BaseModel):
    address: List[FlatchrAddres1] = Field(None, Description="Address1 object")


class FlatchrOrganization(BaseModel):
    communication: FlatchrCommunication1 = Field(
        None, Description="Communication1 object"
    )
    name: Optional[str] = Field(None, Description="Address of the employment")


class FlatchrJobCategory(BaseModel):
    name: Optional[str] = Field(None, Description="Category of the job")


class FlatchrPositionHistory(BaseModel):
    organization: FlatchrOrganization = Field(None, Description="Organization object")
    jobCategories: List[FlatchrJobCategory] = Field(
        [], Description="Category of the job"
    )
    jobLevels: List = Field([], Description="[]")
    start: Optional[str] = Field(
        None, Description="Date of the start of the employment"
    )
    current: Optional[bool] = Field(
        None, Description="Is this employment the current employment"
    )


class FlatchrEmploymentProfile(BaseModel):
    title: Optional[str] = Field(None, Description="Title of the employment")
    positionHistories: List[FlatchrPositionHistory] = Field(
        None, Description="List of the position histories object"
    )
    start: Optional[str] = Field(
        None, Description="Date of the start of the employment"
    )
    current: bool = Field(None, Description="Is this employment the current employment")
    descriptions: List[Optional[str]] = Field(
        [None], Description="Description of the experience"
    )


class FlatchrNameProfile(BaseModel):
    formattedName: Optional[str] = Field("Undefined")
    given: Optional[str] = Field("Undefined")
    family: Optional[str] = Field("Undefined")


class FlatchrPhoneProfile(BaseModel):
    dialNumber: Optional[str] = Field(
        "XXXXX", Description="Number of the phone of the candidat"
    )
    useCode: Optional[str] = Field(None, Description="What kind of phone it is")


class FlatchrEmailProfile(BaseModel):
    address: Optional[str] = Field(None, Description="Email adress of the candidat")


class AddressValue(BaseModel):
    administrative_area_level_1: Optional[str]
    location_lat: Optional[str]
    location_lng: Optional[str]


class FlatchrValueProfile(BaseModel):
    education: Optional[List[FlatchrEducationProfile]] = Field(
        None, Description="List of educations"
    )
    employment: Optional[List[FlatchrEmploymentProfile]] = Field(
        None, Description="List of employments"
    )
    name: Optional[FlatchrNameProfile] = Field(None, Description="Name of the candidat")
    phone: Optional[List[FlatchrPhoneProfile]] = Field(
        "XXXXX", Description="Phone of the candidat"
    )
    email: Optional[List[FlatchrEmailProfile]] = Field(
        None, Description="Email adress of the candidat"
    )
    position: Optional[List[Optional[str]]] = Field(
        None, Description="Position of the candidat"
    )
    employment_positions: Optional[List[Optional[str]]] = Field(
        None, Description="Employment adress of the candidat"
    )
    experience: Optional[int] = Field(
        None, Description="Years of experience of the candidat"
    )


class FlatchrEnrichmentProfile(BaseModel):
    app_name: Optional[str] = Field(
        "HRMatch", Description="App name, 'HRMatch' by default"
    )
    reference: Optional[str] = Field(
        None, Description="Reference of the profile which is the email adress"
    )
    name: Optional[str] = Field("parsing", Description="'Parsing' by default")
    type: Optional[str] = Field("applicants", Description="'applicants by default'")
    value: Optional[FlatchrValueProfile] = Field(
        None, Description="Content of the profile"
    )
