import typing as t

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
    records: list[SalesforceRecordT]


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
    LastModifiedDate: str
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
    Consent_Embedding_Controller__c: t.Optional[IntFlag]
    Consent_Embedding_Owner__c: t.Optional[IntFlag]
    Consent_Parsing_Controller__c: t.Optional[IntFlag]
    Consent_Parsing_Owner__c: t.Optional[IntFlag]
    Consent_Reasoning_Controller__c: t.Optional[IntFlag]
    Consent_Reasoning_Owner__c: t.Optional[IntFlag]
    Consent_Revealing_Controller__c: t.Optional[IntFlag]
    Consent_Revealing_Owner__c: t.Optional[IntFlag]
    Consent_Scoring_Controller__c: t.Optional[IntFlag]
    Consent_Scoring_Owner__c: t.Optional[IntFlag]
    Consent_Searching_Controller__c: t.Optional[IntFlag]
    Consent_Searching_Owner__c: t.Optional[IntFlag]
    Courses__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Dataset_Id__c: int
    Date_Birth__c: t.Optional[str]
    Date_Edition__c: t.Optional[str]
    Date_Reception__c: t.Optional[str]
    Educations_Duration__c: t.Optional[float]
    Email__c: t.Optional[str]
    Experiences_Duration__c: t.Optional[float]
    Filename__c: t.Optional[str]
    First_Name__c: t.Optional[str]
    Gender__c: t.Optional[Gender]
    Hash_Id__c: str
    Id__c: int
    Interests__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Item_Key__c: t.Optional[str]
    Item_Type__c: t.Optional[str]
    Item_URL__c: t.Optional[str]
    Labels__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Languages__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Last_Name__c: t.Optional[str]
    Location_Fields__c: Json
    Location_Gmaps__c: t.Optional[str]
    Location_Lat__c: t.Optional[float]
    Location_Lng__c: t.Optional[float]
    Location_Text__c: str
    Metadatas__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Miscellaneous__c: t.Optional[str]
    Name__c: t.Optional[str]
    Notification__c: t.Optional[IntFlag]
    Phone__c: t.Optional[str]
    Picture__c: t.Optional[str]
    Reference__c: t.Optional[str]
    Scoring_Date_Edition__c: t.Optional[str]
    Scoring_Enabled__c: IntFlag
    Searching_Date_Edition__c: t.Optional[str]
    Searching_Enabled__c: IntFlag
    Seniority__c: t.Optional[Seniority]
    Skills__c: t.Optional[Json[t.List[Skill]]]
    Status_Embedding__c: t.Optional[Status]
    Status_Parsing__c: t.Optional[Status]
    Status_Revealing__c: t.Optional[Status]
    Status_Searching__c: t.Optional[Status]
    Summary__c: t.Optional[str]
    Tags__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Tasks__c: t.Optional[Json[t.List[GeneralEntitySchema]]]
    Text_Language__c: t.Optional[str]
    Text__c: t.Optional[str]
    Timestamp__c: t.Optional[str]
    URLs__c: t.Optional[URLs]
    Viewers__c: t.Optional[Json[t.List]]
