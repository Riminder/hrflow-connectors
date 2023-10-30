from typing import List

from pydantic import BaseModel


class JobOpenings(BaseModel):
    AccountManager: str
    PostingTitle: str
    ClientName: str
    JobOpeningID: str
    AssignedRecruiter: str
    ClientName: str
    ContactName: str
    Rate: float
    TargetDate: str
    NumberOfPositions: str
    Stage: str
    Probability: int
    ExpectedRevenue: float
    Others: List[str]
    JobOpeningStatus: str
    DateOpened: str
    Industry: str
    JobType: str
    State: str
    City: str
    WorkExperience: str
    Country: str
    ModifiedBy: str
    Salary: str
    JobDescription: str
    JobSummary: List[str]


class Candidate(BaseModel):
    candidateOwner: str
    salutation: str
    firstName: str
    lastName: str
    candidateID: str
    experienceInYears: float
    source: str
    highestQualificationHeld: str
    currentJobTitle: str
    phone: str
    mobile: str
    fax: str
    email: str
    currentEmployer: str
    skypeID: str
    website: str
    candidateStatus: str
    expectedSalary: float
    currentSalary: float
    emailOptOut: bool
    skillSet: str
    street: str
    city: str
    state: str
    zipCode: str
    country: str
    additionalInfo: str
    twitter: str
    modifiedBy: str
    createdBy: str
    resume: str
    formattedResume: str
    coverLetter: str
    others: str
