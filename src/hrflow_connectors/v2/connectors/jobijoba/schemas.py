import typing as t

from msgspec import Struct


class File(Struct):
    fileName: str
    contentType: str
    data: str


class Resume(Struct):
    file: File


class Applicant(Struct):
    firstName: str
    lastName: str
    email: str
    phoneNumber: str


class Job(Struct):
    jobId: str
    jobAtsUrl: str


class JobApplication(Struct):
    applicationId: str
    job: Job
    applicant: Applicant
    resume: Resume
    source: t.Union[str, dict]
    statusApiUrl: str
