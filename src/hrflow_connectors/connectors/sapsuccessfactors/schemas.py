from typing import Optional, Dict, Any, List
from pydantic import BaseModel

# Job model
class SAPSuccessFactorsJobRequistion(BaseModel):
    annual_SA: str
    location: Optional[str]
    city: Optional[str] = None
    country: Optional[str] = None
    department: Optional[str] = None
    division: Optional[str] = None
    facility: Optional[str] = None
    function: Optional[str] = None
    industry: Optional[str] = None
    monthly_salary: Optional[str] = None
    salaryBase: Optional[str] = None
    otherBonus: Optional[str] = None
    salaryMax: Optional[str] = None
    salaryMin: Optional[str] = None
    stateProvince: Optional[str] = None
    jobStartDate: Optional[str] = None
    recruiterTeam: Optional[Dict[str, Any]] = None
    hiringManagerTeam: Optional[Dict[str, Any]] = None
    sourcerTeam: Optional[Dict[str, Any]] = None


class SAPSuccessFactorsJob(BaseModel):
    jobDescription: str
    jobTitle: str
    jobReqId: str
    jobRequisition: SAPSuccessFactorsJobRequistion


# Profile model


class Result(BaseModel):

    endDate: str
    school: str
    schoolAddress: str
    startDate: str


class Education(BaseModel):
    results: List[Result]


class ResultLanguage(BaseModel):
    language: str
    readingProf: str
    speakingProf: str
    writingProf: str


class Languages(BaseModel):
    results: List[ResultLanguage]


class ResultOutsideWorkExperience(BaseModel):
    employer: str
    employerAddress: str
    endDate: str
    startDate: str


class OutsideWorkExperience(BaseModel):
    results: List[ResultOutsideWorkExperience]


class InsideWorkExperienceResult(BaseModel):
    backgroundElementId: str
    bgOrderPos: str
    candidateId: str
    department: str
    endDate: str
    lastModifiedDateTime: str
    startDate: str
    title: str
    candidate: str


class InsideWorkExperience(BaseModel):
    results: List[InsideWorkExperienceResult]


class TalentPoolResults(BaseModel):
    startDate: str
    talentPoolComments: str
    talentPoolStatus: str
    talentPoolitem: str


class TalentPool(BaseModel):
    results: List[TalentPoolResults]


class SapCandidateModel(BaseModel):
    address: str
    cellPhone: Optional[str]
    city: Optional[str]
    contactEmail: Optional[str]
    country: str
    creationDateTime: str
    currentTitle: Optional[str]
    dateofAvailability: Optional[str]
    firstName: str
    homePhone: Optional[str]
    lastName: str
    middleName: Optional[str]
    partnerMemberId: Optional[str]
    partnerSource: Optional[str]
    primaryEmail: str
    zip: Optional[str]
    education: Education
    languages: Languages
    outsideWorkExperience: Optional[OutsideWorkExperience]
    insideWorkExperience: Optional[InsideWorkExperience]
    talentPool: Optional[TalentPool]
