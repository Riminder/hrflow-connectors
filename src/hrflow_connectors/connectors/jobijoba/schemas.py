import typing as t

from pydantic import BaseModel


class File(BaseModel):
    fileName: str
    contentType: str
    data: str


class Resume(BaseModel):
    file: File


class Applicant(BaseModel):
    firstName: str
    lastName: str
    email: str
    phoneNumber: str


class Job(BaseModel):
    jobId: str
    jobAtsUrl: str


class JobApplication(BaseModel):
    applicationId: str
    job: Job
    applicant: Applicant
    resume: Resume
    source: t.Union[str, dict]
    statusApiUrl: str
