import typing as t

try:
    t.Literal
except AttributeError:
    from typing_extensions import Literal

    setattr(t, "Literal", Literal)

from pydantic import BaseModel, Json
from pydantic.generics import GenericModel


class Attributes(BaseModel):
    type: str
    url: str


class SalesforceRecord(BaseModel):
    attributes: Attributes


SalesforceRecordT = t.TypeVar("SalesforceRecordT", bound=SalesforceRecord)


class SalesforceRelationship(GenericModel, t.Generic[SalesforceRecordT]):
    totalSize: int
    done: bool
    records: t.List[SalesforceRecordT]


class GeneralEntitySchema(BaseModel):
    name: str
    value: t.Optional[str]


class Skill(BaseModel):
    name: str
    type: t.Optional[str]
    value: t.Optional[str]


class SalesforceExperience(SalesforceRecord):
    Certifications__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Company__c: t.Optional[str]
    Courses__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Date_Begin__c: t.Optional[str]
    Date_End__c: t.Optional[str]
    Description__c: t.Optional[str]
    Hash_Id__c: str
    Id__c: int
    Location_Fields__c: Json
    Location_Gmaps__c: t.Optional[str]
    Location_Lat__c: t.Optional[float]
    Location_Lng__c: t.Optional[float]
    Location_Text__c: str
    Profile__c: str
    Skills__c: t.Optional[Json[t.List[Skill]]]
    Tasks__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Title__c: t.Optional[str]


class SalesforceEducation(SalesforceRecord):
    Certifications__c: Json[t.List[GeneralEntitySchema]]
    Courses__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Date_Begin__c: t.Optional[str]
    Date_End__c: t.Optional[str]
    Description__c: t.Optional[str]
    Hash_Id__c: str
    Id__c: int
    Location_Fields__c: Json
    Location_Gmaps__c: t.Optional[str]
    Location_Lat__c: t.Optional[float]
    Location_Lng__c: t.Optional[float]
    Location_Text__c: str
    Profile__c: str
    School__c: t.Optional[str]
    Skills__c: t.Optional[Json[t.List[Skill]]]
    Tasks__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Title__c: t.Optional[str]


class SalesforceAttachment(SalesforceRecord):
    Alt__c: str
    Date_Edition__c: t.Optional[str]
    Extension__c: str
    File_Name__c: str
    File_Size__c: int
    Id__c: int
    Original_File_Name__c: str
    Profile__c: str
    Text__c: t.Optional[str]
    Timestamp__c: str
    Type__c: str
    URL__c: str


IntFlag = t.Literal[0, 1]
Gender = t.Literal["male", "M", "female", "F", "U", ""]
Seniority = t.Literal["senior", "junior", "dev"]
Status = t.Literal["inactive"]


class URLs(BaseModel):
    from_resume: t.Optional[t.List[str]]
    linkedin: t.Optional[str]
    twitter: t.Optional[str]
    facebook: t.Optional[str]
    github: t.Optional[str]


class SalesforceHrFlowProfile(SalesforceRecord):
    HrFlow_Profile_Experiences__r: t.Optional[
        SalesforceRelationship[SalesforceExperience]
    ]
    HrFlow_Profile_Educations__r: t.Optional[
        SalesforceRelationship[SalesforceEducation]
    ]
    HrFlow_Profile_Attachments__r: t.Optional[
        SalesforceRelationship[SalesforceAttachment]
    ]
    Archive__c: t.Optional[str]
    Certifications__c: Json[t.List[GeneralEntitySchema]]
    Courses__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Dataset_Id__c: int
    Date_Birth__c: t.Optional[str]
    Date_Edition__c: t.Optional[str]
    Date_Reception__c: t.Optional[str]
    Educations_Duration__c: t.Optional[float]
    Email__c: t.Optional[str]
    Experiences_Duration__c: t.Optional[float]
    First_Name__c: t.Optional[str]
    Gender__c: t.Optional[Gender]
    Hash_Id__c: str
    Id__c: int
    Interests__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Labels__c: t.Optional[Json]
    Languages__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    LastModifiedDate: str
    Last_Name__c: t.Optional[str]
    Location_Fields__c: Json
    Location_Gmaps__c: t.Optional[str]
    Location_Lat__c: t.Optional[float]
    Location_Lng__c: t.Optional[float]
    Location_Text__c: str
    Metadatas__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Name__c: t.Optional[str]
    Phone__c: t.Optional[str]
    Picture__c: t.Optional[str]
    Reference__c: t.Optional[str]
    Seniority__c: t.Optional[Seniority]
    Skills__c: t.Optional[Json[t.List[Skill]]]
    Summary__c: t.Optional[str]
    Tags__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Tasks__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Text_Language__c: t.Optional[str]
    Text__c: t.Optional[str]
    URLs__c: t.Optional[Json[URLs]]


class Section(BaseModel):
    name: str
    title: str
    description: str


class RangeFloat(BaseModel):
    name: str
    value_min: float
    value_max: float
    unit: str


class RangeDate(BaseModel):
    name: str
    value_min: str
    value_max: str


class SalesforceHrFlowJob(SalesforceRecord):
    Archive__c: t.Optional[str]
    Benefits__c: t.Optional[str]
    Board_Id__c: int
    Certifications__c: Json[t.List[GeneralEntitySchema]]
    Courses__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Culture__c: t.Optional[str]
    Date_Edition__c: t.Optional[str]
    Hash_Id__c: str
    Id__c: int
    Interviews__c: t.Optional[str]
    Languages__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    LastModifiedDate: str
    Location_Fields__c: Json
    Location_Gmaps__c: t.Optional[str]
    Location_Lat__c: t.Optional[float]
    Location_Lng__c: t.Optional[float]
    Location_Text__c: str
    Metadatas__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Name__c: str
    Picture__c: t.Optional[str]
    Ranges_Date__c: t.List[RangeDate]
    Ranges_Float__c: t.List[RangeFloat]
    Reference__c: t.Optional[str]
    Requirements__c: t.Optional[str]
    Responsibilities__c: t.Optional[str]
    Sections__c: t.List[Section]
    Skills__c: t.Optional[Json[t.List[Skill]]]
    Slug__c: str
    Status__c: IntFlag
    Summary__c: t.Optional[str]
    Tags__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Tasks__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    URL__c: t.Optional[str]
