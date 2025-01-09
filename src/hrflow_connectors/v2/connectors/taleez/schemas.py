import typing as t
from enum import Enum

from msgspec import Struct


class Property(Struct):
    id: int
    internal: str
    apiKey: str
    value: str
    values: str


class candidateLocation(Struct):
    country: str
    lat: int
    lng: int
    city: str
    postalCode: str
    street: str
    streetNumber: str


class Candidate(Struct):
    id: int
    dateCreation: int
    firstName: str
    lastName: str
    mail: str
    phone: str
    cv: str
    lang: str
    socialLinks: t.List[t.Dict]
    unitId: int
    properties: t.Optional[t.List[t.Dict]]
    location: candidateLocation


class JobStatus(str, Enum):
    draft = "DRAFT"
    published = "PUBLISHED"
    done = "DONE"
    suspended = "SUSPENDED"


class ContractType(str, Enum):
    cdi = "CDI"
    cdd = "CDD"
    interim = "INTERIM"
    freelance = "FREELANCE"
    internship = "INTERNSHIP"
    apprenticeship = "APPRENTICESHIP"
    student = "STUDENT"
    vie = "VIE"
    franchise = "FRANCHISE"
    statute = "STATUTE"
    vacataire = "VACATAIRE"
    liberal = "LIBERAL"
    cdi_chantier = "CDI_CHANTIER"
    intermittent = "INTERMITTENT"
    season = "SEASON"
    other = "OTHER"
    volunteer = "VOLUNTEER"
    permenant = "PERMENANT"
    fixedterm = "FIXEDTERM"


class JobVisibility(str, Enum):
    public = "PUBLIC"
    internal = "INTERNAL"
    internal_and_public = "INTERNAL_AND_PUBLIC"
    private = "PRIVATE"


class Job(Struct):
    id: int
    token: str
    dateCreation: int
    dateFirstPublish: int
    dateLastPublish: int
    label: t.Optional[str]
    currentStatus: JobStatus
    contract: t.Optional[ContractType]
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
    visibility: JobVisibility
    jobDescription: str
    profileDescription: str
    companyDescription: str
    properties: t.List[t.Dict]


class JobProperty(Struct):
    key: str
    value: str
    values: t.List[str]
