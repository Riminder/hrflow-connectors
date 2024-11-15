import typing as t

from pydantic import BaseModel


class Property(BaseModel):
    id: int
    key: str
    name: str
    nameEn: str
    type: str
    choices: t.List[t.Dict]


class Candidate(BaseModel):
    id: int
    firstName: str
    lastName: str
    mail: str
    initialReferrer: str
    lang: str
    socialLinks: dict
    properties: t.Optional[list]
    jobs: t.Optional[list]


class Job(BaseModel):
    id: int
    token: str
    dateCreation: int
    dateFirstPublish: int
    dateLastPublish: int
    label: str
    currentStatus: str
    contract: str
    contractLength: int
    fullTime: bool
    workHours: int
    remote: bool
    country: str
    city: str
    postalCode: str
    lat: str
    lng: str
    recruiterId: int
    who: str
    logo: str
    banner: str
    companyLabel: str
    tags: t.List[t.Dict]
    url: str
    urlApplying: str
    jobDescription: str
    profileDescription: str
    companyDescription: str
    properties: t.List[t.Dict]
    public: bool


class JobProperty(BaseModel):
    key: str
    value: str
    values: t.List[str]
