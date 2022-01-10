from typing import Optional, Dict, Any, Union, List
from pydantic import BaseModel


class FlatchrResume(BaseModel):
    fileName: str
    contentType: str
    data: str


class FlatchrAnswers(BaseModel):
    question: Union[str, int]
    value: str


class FlatchrCreationProfile(BaseModel):
    vacancy: str
    firstname: str
    lastname: str
    type: str
    token: str
    resume: FlatchrResume
    email: str
    phone: Optional[str]
    comment: Optional[str]
    offerer_id: Optional[int]
    legalNewsletterPartners: Optional[bool]
    similarities: Optional[bool]
    response_text: Optional[str]
    answers: Optional[FlatchrAnswers]


class FlatchrAdressProfile(BaseModel):
    formattedAddress: str


class FlatchrCommunicationProfile(BaseModel):
    address: List[FlatchrAdressProfile]


class FlatchrInstitutionProfile(BaseModel):
    communication: FlatchrCommunicationProfile
    name: str


class FlatchrEducationLevelCode(BaseModel):
    name: str


class FlatchrEducationDegree(BaseModel):
    name: str
    date: str
    specializations: List


class FlatchrEducationProfile(BaseModel):
    institution: FlatchrInstitutionProfile
    educationLevelCodes: List[FlatchrEducationLevelCode]
    educationDegrees: List[FlatchrEducationDegree]
    end: str
    descriptions: List[str]


class FlatchrGeoLocation(BaseModel):
    latitude: float
    longitude: float


class FlatchrAddres1(BaseModel):
    countryCode: str
    city: str
    postalCode: str
    geoLocation: FlatchrGeoLocation
    formattedAddress: str


class FlatchrCommunication1(BaseModel):
    address: List[FlatchrAddres1]


class FlatchrOrganization(BaseModel):
    communication: FlatchrCommunication1
    name: str


class FlatchrJobCategory(BaseModel):
    name: str


class FlatchrPositionHistory(BaseModel):
    organization: FlatchrOrganization
    jobCategories: List[FlatchrJobCategory]
    jobLevels: List
    start: str
    current: bool


class FlatchrEmploymentProfile(BaseModel):
    title: str
    positionHistories: List[FlatchrPositionHistory]
    start: str
    current: bool
    descriptions: List[str]


class FlatchrNameProfile(BaseModel):
    formattedName: str
    given: str
    family: str


class FlatchrPhoneProfile(BaseModel):
    dialNumber: str
    useCode: str


class FlatchrEmailProfile(BaseModel):
    address: str


class FlatchrValueProfile(BaseModel):
    education: List[FlatchrEducationProfile]
    employment: List[FlatchrEmploymentProfile]
    name: FlatchrNameProfile
    phone: List[FlatchrPhoneProfile]
    email: List[FlatchrEmailProfile]
    position: List[str]
    employment_positions: List[str]
    experience: int


class FlatchrEnrichmentProfile(BaseModel):
    app_name: str
    reference: str
    name: str
    type: str
    value: FlatchrValueProfile
