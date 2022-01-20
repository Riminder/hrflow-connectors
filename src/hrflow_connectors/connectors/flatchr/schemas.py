from typing import Optional, Union, List
from pydantic import BaseModel, Field


class FlatchrResume(BaseModel):
    fileName: str = Field(..., Description="Name of the file")
    contentType: str = Field(..., Description="Type of the content of the file")
    data: str = Field(..., Description="Content of the file")


class FlatchrAnswers(BaseModel):
    question: Union[str, int] = Field(..., Description="Question text or tag ID")
    value: str = Field(..., Description="Value of the answer or tag")


class FlatchrCreationProfile(BaseModel):
    vacancy: str = Field(..., Description="Unique identifier of the ad")
    firstname: str = Field(..., Description="First name /[a-zA-Z]/")
    lastname: str = Field(..., Description="Last name /[a-zA-Z]/")
    type: str = Field("applicants", Description="'applicants' by default")
    token: str = Field(
        ..., Description="API key. It is created in the Flatchr interface"
    )
    resume: FlatchrResume = Field(
        ...,
        Description="Type of format for the CV: document (if cv base64, default), "
        "json (if cv, hr-xml format)",
    )
    email: str = Field(
        ...,
        Description="Email will be used as a reference for the profile. The enrichment request "
        "aim the right profile to enrich with the email.",
    )
    phone: Optional[str] = Field(..., Description="Phone")
    comment: Optional[str] = Field(..., Description="Candidate's comment")
    offerer_id: Optional[int] = Field(..., Description="Flatchr login to ask the team")
    legalNewsletterPartners: Optional[bool] = Field(
        ..., Description="Opt-in newsletter"
    )
    similarities: Optional[bool] = Field(..., Description="Return to similar offers")
    response_text: Optional[str] = Field(..., Description="Return text")
    answers: Optional[FlatchrAnswers] = Field(
        ..., Description="Answers to questions/tags"
    )


class FlatchrAdressProfile(BaseModel):
    formattedAddress: str = Field(
        ..., Description="Adress where the education were done"
    )


class FlatchrCommunicationProfile(BaseModel):
    address: List[FlatchrAdressProfile] = Field(
        ..., Description="List of adresses where the education were done"
    )


class FlatchrInstitutionProfile(BaseModel):
    communication: FlatchrCommunicationProfile = Field(
        ..., Description="List of adresses"
    )
    name: str = Field(
        ..., Description="Name of the institution in which the education was provided"
    )


class FlatchrEducationLevelCode(BaseModel):
    name: str = Field(..., Description="Degree of the education")


class FlatchrEducationDegree(BaseModel):
    name: str = Field(..., Description="Degree of the education")
    date: str = Field(..., Description="Date on which the degree was obtained")
    specializations: List = Field(..., Description="Specializations of the degree")


class FlatchrEducationProfile(BaseModel):
    institution: FlatchrInstitutionProfile = Field(
        ..., Description="Institution object"
    )
    educationLevelCodes: List[FlatchrEducationLevelCode] = Field(
        ..., Description="List of all the degrees"
    )
    educationDegrees: List[FlatchrEducationDegree] = Field(
        ..., Description="List of all the degrees"
    )
    end: str = Field(..., Description="Date on which the degree has ended")
    descriptions: List[str] = Field(..., Description="Description of the education")


class FlatchrGeoLocation(BaseModel):
    latitude: float = Field(..., Description="Latitude of the employment location")
    longitude: float = Field(..., Description="Longitude of the employment location")


class FlatchrAddres1(BaseModel):
    countryCode: str = Field(..., Description="Country code of the employment location")
    city: str = Field(..., Description="City of the employment location")
    postalCode: str = Field(..., Description="Postalcode of the employment location")
    geoLocation: FlatchrGeoLocation = Field(
        ..., Description="GeoLocation object of the employment location"
    )
    formattedAddress: str = Field(..., Description="Address of the employment location")


class FlatchrCommunication1(BaseModel):
    address: List[FlatchrAddres1] = Field(..., Description="Address1 object")


class FlatchrOrganization(BaseModel):
    communication: FlatchrCommunication1 = Field(
        ..., Description="Communication1 object"
    )
    name: str = Field(..., Description="Address of the employment")


class FlatchrJobCategory(BaseModel):
    name: str = Field(..., Description="Category of the job")


class FlatchrPositionHistory(BaseModel):
    organization: FlatchrOrganization = Field(..., Description="Organization object")
    jobCategories: List[FlatchrJobCategory] = Field(
        ..., Description="Category of the job"
    )
    jobLevels: List = Field([], Description="[]")
    start: str = Field(..., Description="Date of the start of the employment")
    current: bool = Field(..., Description="Is this employment the current employment")


class FlatchrEmploymentProfile(BaseModel):
    title: str = Field(..., Description="Title of the employment")
    positionHistories: List[FlatchrPositionHistory] = Field(
        ..., Description="List of the position histories object"
    )
    start: str = Field(..., Description="Date of the start of the employment")
    current: bool = Field(..., Description="Is this employment the current employment")
    descriptions: List[str] = Field(..., Description="Description of the experience")


class FlatchrNameProfile(BaseModel):
    formattedName: str
    given: str
    family: str


class FlatchrPhoneProfile(BaseModel):
    dialNumber: str = Field(..., Description="Number of the phone of the candidat")
    useCode: str = Field(..., Description="What kind of phone it is")


class FlatchrEmailProfile(BaseModel):
    address: str = Field(..., Description="Email adress of the candidat")


class FlatchrValueProfile(BaseModel):
    education: List[FlatchrEducationProfile] = Field(
        ..., Description="List of educations"
    )
    employment: List[FlatchrEmploymentProfile] = Field(
        ..., Description="List of employments"
    )
    name: FlatchrNameProfile = Field(..., Description="Name of the candidat")
    phone: List[FlatchrPhoneProfile] = Field(..., Description="Phone of the candidat")
    email: List[FlatchrEmailProfile] = Field(
        ..., Description="Email adress of the candidat"
    )
    position: List[str] = Field(..., Description="Position of the candidat")
    employment_positions: List[str] = Field(
        ..., Description="Employment adress of the candidat"
    )
    experience: int = Field(..., Description="Years of experience of the candidat")


class FlatchrEnrichmentProfile(BaseModel):
    app_name: str = Field("HRMatch", Description="App name, 'HRMatch' by default")
    reference: str = Field(
        ..., Description="Reference of the profile which is the email adress"
    )
    name: str = Field("parsing", Description="'Parsing' by default")
    type: str = Field("applicants", Description="'applicants by default'")
    value: FlatchrValueProfile = Field(..., Description="Content of the profile")
