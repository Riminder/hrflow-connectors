import typing as t
from pydantic import BaseModel

class Property(BaseModel):
    id: int
    key: str
    name: str
    nameEn: str
    type: str
    choices: t.List[t.Dict]


class Job(BaseModel):
    id: int
    token: str
    dateCreation: int
    dateFirstPublish: int
    dateLastPublish: int


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
