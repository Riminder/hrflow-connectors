from typing import List, Optional

from pydantic import BaseModel


class ProfileLocation(BaseModel):
    address1: Optional[str]
    address2: Optional[str]
    city: Optional[str]
    zip: Optional[str]
    regionName: Optional[str]
    homePhone: Optional[str]
    workPhone: Optional[str]
    mobilePhone: Optional[str]
    fax: Optional[str]
    alternateEmail: Optional[str]
    personalWebSite: Optional[str]
    preferredComChannel: Optional[str]
    countryName: Optional[str]


class Reference(BaseModel):
    employeeCode: Optional[str]
    company: Optional[str]
    email: Optional[str]
    referredFrom: Optional[str]


class PersonalData(BaseModel):
    dateOfBirth: Optional[int]
    numberOfChildren: Optional[int]
    citizenship: Optional[str]
    language: Optional[str]
    maritalStatus: Optional[str]
    sex: Optional[str]


class User(BaseModel):
    id: Optional[int]
    login: Optional[str]
    fullName: Optional[str]
    language: Optional[str]


class Position(BaseModel):
    company: Optional[str]
    companyStartDate: Optional[int]
    position: Optional[str]
    positionStartDate: Optional[int]
    positionEndDate: Optional[int]
    managerName: Optional[str]
    employeeCode: Optional[str]
    compensation: Optional[str]
    location: Optional[str]


class ConsentId(BaseModel):
    id: Optional[int]


class BasicCandidateConsent(BaseModel):
    consentId: ConsentId
    consentType: Optional[str]
    status: str
    comment: Optional[str]

    class Config:
        extra = "allow"


class BasicTagCategory(BaseModel):
    name: Optional[str]

    class Config:
        extra = "allow"


class BasicCandidateTag(BaseModel):
    id: Optional[int]
    name: Optional[str]
    icon: Optional[str]
    category: BasicTagCategory

    class Config:
        extra = "allow"


class BasicCampaignType(BaseModel):
    id: Optional[int]

    class Config:
        extra = "allow"


class Referrer(BaseModel):
    firstName: Optional[str]
    lastName: Optional[str]
    email: Optional[str]
    employeeCode: Optional[str]
    employeeCompany: Optional[str]


class Method(str, enum.Enum):
    GET = "GET"
    POST = "POST"
    PUT = "PUT"
    DELETE = "DELETE"
    PATCH = "PATCH"


class URL(BaseModel):
    href: str
    method: Method


class Attachment(BaseModel):
    id: int
    mimeType: str
    fileName: str
    description: str
    fileUrl: URL
    author: str
    creationDate: Timestamp
    type: str
    documentGroup: str


class DocumentBase(BaseModel):
    id: int
    name: str
    description: str
    documentGroup: str
    fileUrl: URL
    type: str
    author: str
    creationDate: Timestamp
    updateDate: Timestamp


class TalentLinkProfile(BaseModel):
    id: Optional[int]
    type: Optional[str]
    contractor: bool = False
    initialType: Optional[str]
    firstname: str
    lastname: str
    middlename: Optional[str]
    email: Optional[str]
    academicTitle: Optional[str]
    srcChannelName: Optional[str]
    origin: Optional[str]
    creation: Optional[int]
    update: Optional[int]
    socialSecurityNumber: Optional[str]
    anonymous: bool = False
    formOfAddress: Optional[str]
    address: Optional[ProfileLocation]
    reference: Optional[Reference]
    personalData: Optional[PersonalData]
    creationUser: User
    updateUser: User
    sourceChannelType: Optional[str]
    status: Optional[str]
    sourcingMedium: Optional[str]
    uiLanguage: Optional[str]
    dataPrivacyStatement: Optional[str]
    expectedArchiving: Optional[int]
    memo: Optional[str]
    linkedInId: Optional[str]
    partiallyDeleted: bool
    latitude: Optional[float]
    longitude: Optional[float]
    internalCandidateId: Optional[str]
    position: Optional[Position]
    candidateConsents: List[BasicCandidateConsent]
    tags: Optional[List[BasicCandidateTag]]
    openingId: Optional[int]
    poolId: Optional[int]
    campaignTypes: Optional[List[BasicCampaignType]]
    referrer: Optional[Referrer]
    attachments: Optional[List[Attachment]]
    documents: Optional[List[DocumentBase]]
    # applications
    # SocialNetworks


class TalentLinkJob(BaseModel):
    """"""


class TalentLinkApplication(BaseModel):
    """"""


class TalentLinkScore(BaseModel):
    """"""


candidateFullDto = "id, actions, type, contractor, initialType, firstname, lastname, middlename, email, academicTitle, srcChannelName, origin, creation, update, socialSecurityNumber, anonymous, formOfAddress, address, position, reference, personalData, creationUser, updateUser, sourceChannelType, status, sourcingMedium, uiLanguage, dataPrivacyStatement, talentDatabaseConsent, expectedArchiving, memo, linkedInId, partiallyDeleted, latitude, longitude, archivedManually, sendDeletionNotification, inactiveDate, archiveDateSinceProfileCreated, archiveDateSinceConsentGiven, internalCandidateId, tags, applications, pools, candidateConsents, campaignTypes, attachments, documents, referrals, SocialNetworks"
PublishedAdvertDTO = "id, status, postingStartDate, postingEndDate, creationDate, updateDate, postingUser, navigation, jobboardConfiguration, advertId, jobAdTitle, comment, recruitingCompany, location, defaultJobLocation, applicationProcess, showCompensation, showRecruiter, keyword, language, advertCreationDate, advertExpirationDate, advertUpdateDate, assignedImages, attachments, strapline, opening, customFields, lovs, freeFormFields, actions, jobLocations, technicalId, structuredData, categories, onSite, hybrid, remote,"
ApplicationDTO = "id, sourceChannelName, sourceChannelType, sourcingMedium, creation, update, applicationDate, status, memo, shortListed, archived, activeApplication, hasContracts, completionReason, statusComment, candidate, opening, documents, applicationFollowups, candidateConsents, score, attachments, "
