from typing import Optional, List
from pydantic import BaseModel, Field

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
    outsideWorkExperience: OutsideWorkExperience
    talentPool: Optional[TalentPool]