import enum
import json
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.talentlink.schemas import (
    TalentLinkApplication,
    TalentLinkJob,
    TalentLinkProfile,
    TalentLinkScore,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

TALENTLINK_CANDIDATES_ENDPOINT = (
    "https://apiproxy.shared.lumessetalentlink.com/tlk/rest/candidate"
)

TALENTLINK_JOBS_ENDPOINT = (
    "https://apiproxy.shared.lumessetalentlink.com/tlk/rest/publishedadvert"
)

TALENTLINK_APPLICATIONS_ENDPOINT = (
    "https://apiproxy.shared.lumessetalentlink.com/tlk/rest/application"
)


class SortOrder(str, enum.Enum):
    ascending = "ASC"
    descending = "DESC"


class CandidateSortFields(str, enum.Enum):
    id = "id"
    type = "type"
    initialType = "initialType"
    firstname = "firstname"
    lastname = "lastname"
    email = "email"
    srcChannelName = "srcChannelName"
    creation = "creation"
    update = "update"
    status = "status"
    sourcingMedium = "sourcingMedium"


class CandidateFilter(ParametersModel):
    id_in: t.List[int] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_eq: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_gt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_ge: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_lt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_le: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    id_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    status_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    status_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    status_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    status_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    firstname_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    firstname_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    firstname_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    firstname_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    lastname_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    lastname_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    lastname_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    lastname_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    email_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    email_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    email_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    email_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_gt: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_ge: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_lt: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_le: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    update_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    type_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    type_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    type_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    type_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_gt: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_ge: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_lt: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_le: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    creation_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourcingMedium_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourcingMedium_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourcingMedium_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourcingMedium_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourceChannelType_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourceChannelType_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourceChannelType_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    sourceChannelType_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    dataPrivacyStatement_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    dataPrivacyStatement_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    dataPrivacyStatement_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    dataPrivacyStatement_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    internalCandidateId_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    internalCandidateId_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    internalCandidateId_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    internalCandidateId_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    socialSecurityNumber_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    socialSecurityNumber_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    socialSecurityNumber_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    socialSecurityNumber_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address1_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address1_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address1_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address1_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address2_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address2_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address2_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    address2_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    city_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    city_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    city_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    city_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    zip_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    zip_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    zip_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    zip_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    regionName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    regionName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    regionName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    regionName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    homePhone_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    homePhone_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    homePhone_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    homePhone_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    workPhone_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    workPhone_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    workPhone_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    workPhone_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    mobilePhone_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    mobilePhone_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    mobilePhone_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    mobilePhone_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    fax_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    fax_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    fax_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    fax_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    alternateEmail_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    alternateEmail_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    alternateEmail_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    alternateEmail_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    personalWebSite_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    personalWebSite_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    personalWebSite_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    personalWebSite_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    preferredComChannel_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    preferredComChannel_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    preferredComChannel_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    preferredComChannel_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    countryName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    countryName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    countryName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    countryName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    tagName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    tagName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    tagName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    tagName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_eq: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_gt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_ge: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_lt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_le: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationId_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelType_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelType_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelType_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationSourceChannelType_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationStatus_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationStatus_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationStatus_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationStatus_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_eq: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_gt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_ge: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_lt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_le: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    openingId_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobNumber_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobNumber_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobNumber_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobNumber_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobTitle_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobTitle_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobTitle_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    jobTitle_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    poolName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    poolName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    poolName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    poolName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentType_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentType_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentType_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentType_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentStatus_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentStatus_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentStatus_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    consentStatus_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_eq: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_gt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_ge: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_lt: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_le: int = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    departmentId_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    applicationScore_eq: float
    applicationScore_gt: float
    applicationScore_ge: float
    applicationScore_lt: float
    applicationScore_le: float
    applicationScore_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    attachmentFileName_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    attachmentFileName_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    attachmentFileName_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    attachmentFileName_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentGroup_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentGroup_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentGroup_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentType_eq: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentType_like: str = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentType_in: t.List[str] = Field(
        None,
        field_type=FieldType.QueryParam,
    )
    documentType_isNull: bool = Field(
        None,
        field_type=FieldType.QueryParam,
    )


class PublishedAdvertSortFields(str, enum.Enum):
    id = "id"
    status = "status"
    postingStartDate = "postingStartDate"
    postingEndDate = "postingEndDate"
    creationDate = "creationDate"
    updateDate = "updateDate"
    advertId = "advertId"
    technicalId = "technicalId"


class PublishedAdvertsFilter(ParametersModel):
    id_in: t.List[int]
    id_eq: int
    id_gt: int
    id_ge: int
    id_lt: int
    id_le: int
    id_isNull: bool
    departmentId_eq: int
    departmentId_gt: int
    departmentId_ge: int
    departmentId_lt: int
    departmentId_le: int
    departmentId_isNull: bool
    status_eq: str
    status_like: str
    status_in: t.List[str]
    status_isNull: bool
    postingStartDate_eq: str
    postingStartDate_gt: str
    postingStartDate_ge: str
    postingStartDate_lt: str
    postingStartDate_le: str
    postingStartDate_isNull: bool
    postingEndDate_eq: str
    postingEndDate_gt: str
    postingEndDate_ge: str
    postingEndDate_lt: str
    postingEndDate_le: str
    postingEndDate_isNull: bool
    creationDate_eq: str
    creationDate_gt: str
    creationDate_ge: str
    creationDate_lt: str
    creationDate_le: str
    creationDate_isNull: bool
    updateDate_eq: str
    updateDate_gt: str
    updateDate_ge: str
    updateDate_lt: str
    updateDate_le: str
    updateDate_isNull: bool
    advertId_eq: int
    advertId_gt: int
    advertId_ge: int
    advertId_lt: int
    advertId_le: int
    advertId_isNull: bool
    jobAdTitle_eq: str
    jobAdTitle_like: str
    jobAdTitle_in: t.List[str]
    jobAdTitle_isNull: bool
    comment_eq: str
    comment_like: str
    comment_in: t.List[str]
    comment_isNull: bool
    recruitingCompany_eq: str
    recruitingCompany_like: str
    recruitingCompany_in: t.List[str]
    recruitingCompany_isNull: bool
    location_eq: str
    location_like: str
    location_in: t.List[str]
    location_isNull: bool
    showCompensation_eq: bool
    showCompensation_in: t.List[bool]
    showCompensation_isNull: bool
    showRecruiter_eq: bool
    showRecruiter_in: t.List[bool]
    showRecruiter_isNull: bool
    keyword_eq: str
    keyword_like: str
    keyword_in: t.List[str]
    keyword_isNull: bool
    language_eq: str
    language_like: str
    language_in: t.List[str]
    language_isNull: bool
    advertCreationDate_eq: str
    advertCreationDate_gt: str
    advertCreationDate_ge: str
    advertCreationDate_lt: str
    advertCreationDate_le: str
    advertCreationDate_isNull: bool
    advertExpirationDate_eq: str
    advertExpirationDate_gt: str
    advertExpirationDate_ge: str
    advertExpirationDate_lt: str
    advertExpirationDate_le: str
    advertExpirationDate_isNull: bool
    advertUpdateDate_eq: str
    advertUpdateDate_gt: str
    advertUpdateDate_ge: str
    advertUpdateDate_lt: str
    advertUpdateDate_le: str
    advertUpdateDate_isNull: bool
    openingId_eq: int
    openingId_gt: int
    openingId_ge: int
    openingId_lt: int
    openingId_le: int
    openingId_isNull: bool
    scheduleType_eq: str
    scheduleType_like: str
    scheduleType_in: t.List[str]
    scheduleType_isNull: bool
    contractType_eq: str
    contractType_like: str
    contractType_in: t.List[str]
    contractType_isNull: bool
    jobLocationId_eq: int
    jobLocationId_gt: int
    jobLocationId_ge: int
    jobLocationId_lt: int
    jobLocationId_le: int
    jobLocationId_isNull: bool
    jobLocationName_eq: str
    jobLocationName_like: str
    jobLocationName_in: t.List[str]
    jobLocationName_isNull: bool
    jobLocationCountry_eq: str
    jobLocationCountry_like: str
    jobLocationCountry_in: t.List[str]
    jobLocationCountry_isNull: bool
    jobLocationRegion_eq: str
    jobLocationRegion_like: str
    jobLocationRegion_in: t.List[str]
    jobLocationRegion_isNull: bool
    jobLocationCity_eq: str
    jobLocationCity_like: str
    jobLocationCity_in: t.List[str]
    jobLocationCity_isNull: bool
    jobLocationStreet_eq: str
    jobLocationStreet_like: str
    jobLocationStreet_in: t.List[str]
    jobLocationStreet_isNull: bool
    jobLocationZipCode_eq: str
    jobLocationZipCode_like: str
    jobLocationZipCode_in: t.List[str]
    jobLocationZipCode_isNull: bool
    jobLocationLatitude_eq: float
    jobLocationLatitude_gt: float
    jobLocationLatitude_ge: float
    jobLocationLatitude_lt: float
    jobLocationLatitude_le: float
    jobLocationLatitude_isNull: bool
    jobLocationLongitude_eq: float
    jobLocationLongitude_gt: float
    jobLocationLongitude_ge: float
    jobLocationLongitude_lt: float
    jobLocationLongitude_le: float
    jobLocationLongitude_isNull: bool
    technicalId_eq: str
    technicalId_like: str
    technicalId_in: t.List[str]
    technicalId_isNull: bool
    valueLovId_in: t.List[int]
    valueLovId_eq: t.List[int]


class ApplicationSortFields(str, enum.Enum):
    id = "id"
    sourceChannelName = "sourceChannelName"
    sourceChannelType = "sourceChannelType"
    sourcingMedium = "sourcingMedium"
    creation = "creation"
    update = "update"
    status = "status"
    memo = "memo"
    statusComment = "statusComment"
    completionReason = "completionReason"


class ApplicationFilter(ParametersModel):
    id_in: t.List[int]
    id_eq: int
    id_gt: int
    id_ge: int
    id_lt: int
    id_le: int
    id_isNull: bool
    sourceChannelName_eq: str
    sourceChannelName_like: str
    sourceChannelName_in: t.List[str]
    sourceChannelName_isNull: bool
    sourceChannelType_eq: str
    sourceChannelType_like: str
    sourceChannelType_in: t.List[str]
    sourceChannelType_isNull: bool
    sourcingMedium_eq: str
    sourcingMedium_like: str
    sourcingMedium_in: t.List[str]
    sourcingMedium_isNull: bool
    creation_eq: str
    creation_gt: str
    creation_ge: str
    creation_lt: str
    creation_le: str
    creation_isNull: bool
    update_eq: str
    update_gt: str
    update_ge: str
    update_lt: str
    update_le: str
    update_isNull: bool
    applicationDate_eq: str
    applicationDate_gt: str
    applicationDate_ge: str
    applicationDate_lt: str
    applicationDate_le: str
    applicationDate_isNull: bool
    status_eq: str
    status_like: str
    status_in: t.List[str]
    status_isNull: bool
    memo_eq: str
    memo_like: str
    memo_in: t.List[str]
    memo_isNull: bool
    shortListed_eq: bool
    shortListed_in: t.List[bool]
    shortListed_isNull: bool
    archived_eq: bool
    archived_in: t.List[bool]
    archived_isNull: bool
    activeApplication_eq: bool
    activeApplication_in: t.List[bool]
    activeApplication_isNull: bool
    completionReason_eq: str
    completionReason_like: str
    completionReason_in: t.List[str]
    completionReason_isNull: bool
    statusComment_eq: str
    statusComment_like: str
    statusComment_in: t.List[str]
    statusComment_isNull: bool
    candidateId_eq: int
    candidateId_gt: int
    candidateId_ge: int
    candidateId_lt: int
    candidateId_le: int
    candidateId_isNull: bool
    documentName_eq: str
    documentName_like: str
    documentName_in: t.List[str]
    documentName_isNull: bool
    attachmentFileName_eq: str
    attachmentFileName_like: str
    attachmentFileName_in: t.List[str]
    attachmentFileName_isNull: bool
    documentGroup_eq: str
    documentGroup_in: t.List[str]
    documentGroup_isNull: bool
    documentType_eq: str
    documentType_like: str
    documentType_in: t.List[str]
    documentType_isNull: bool
    hasContracts_eq: bool


class ReadProfileParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="Your API key",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Your Username",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Your Password",
        repr=False,
        field_type=FieldType.Auth,
    )
    candidateFilter: CandidateFilter = Field(
        None,
        description="List of available filters with corresponding parameters",
        field_type=FieldType.QueryParam,
    )
    first: int = Field(
        10,
        description="use it to define amount of records on the page (up to 100 per page)",
        field_type=FieldType.QueryParam,
    )
    after: int = Field(
        0,
        description="use it for pagination (to define the starting point of next page)",
        field_type=FieldType.QueryParam,
    )
    sortBy: CandidateSortFields = Field(
        "id",
        description=(
            "specify which parameter you would like the results to be sorted by."
        ),
        field_type=FieldType.QueryParam,
    )
    orderBy: SortOrder = Field(
        None,
        description=(
            "define how you want the results to be ordered (Ascending / Descending) "
        ),
        field_type=FieldType.QueryParam,
    )


class WriteProfilesParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="Your API key",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Your Username",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Your Password",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="Your API key",
        repr=False,
        field_type=FieldType.Auth,
    )
    companyname: str = Field(
        ...,
        description="Your Company name",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Your Username",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Your Password",
        repr=False,
        field_type=FieldType.Auth,
    )
    publishedAdvertsFilter: PublishedAdvertsFilter = Field(
        None,
        description="List of available filters with corresponding parameters",
        field_type=FieldType.QueryParam,
    )
    first: int = Field(
        10,
        description="use it to define amount of records on the page (up to 100 per page)",
        field_type=FieldType.QueryParam,
    )
    after: int = Field(
        0,
        description="use it for pagination (to define the starting point of next page)",
        field_type=FieldType.QueryParam,
    )
    sortBy: PublishedAdvertSortFields = Field(
        "id",
        description=(
            "specify which parameter you would like the results to be sorted by."
        ),
        field_type=FieldType.QueryParam,
    )
    orderBy: SortOrder = Field(
        None,
        description=(
            "define how you want the results to be ordered (Ascending / Descending) "
        ),
        field_type=FieldType.QueryParam,
    )


class ReadApplicationsParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="Your API key",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Your Username",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Your Password",
        repr=False,
        field_type=FieldType.Auth,
    )
    applicationFilter: ApplicationFilter = Field(
        None,
        description="List of available filters with corresponding parameters",
        field_type=FieldType.QueryParam,
    )
    first: int = Field(
        10,
        description="use it to define amount of records on the page (up to 100 per page)",
        field_type=FieldType.QueryParam,
    )
    after: int = Field(
        0,
        description="use it for pagination (to define the starting point of next page)",
        field_type=FieldType.QueryParam,
    )
    sortBy: ApplicationSortFields = Field(
        "id",
        description=(
            "specify which parameter you would like the results to be sorted by."
        ),
        field_type=FieldType.QueryParam,
    )
    orderBy: SortOrder = Field(
        None,
        description=(
            "define how you want the results to be ordered (Ascending / Descending) "
        ),
        field_type=FieldType.QueryParam,
    )


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:

    # definition of fields available in the response
    candidateFullDto = "id, actions, type, contractor, initialType, firstname, lastname, middlename, email, academicTitle, srcChannelName, origin, creation, update, socialSecurityNumber, anonymous, formOfAddress, address, position, reference, personalData, creationUser, updateUser, sourceChannelType, status, sourcingMedium, uiLanguage, dataPrivacyStatement, talentDatabaseConsent, expectedArchiving, memo, linkedInId, partiallyDeleted, latitude, longitude, archivedManually, sendDeletionNotification, inactiveDate, archiveDateSinceProfileCreated, archiveDateSinceConsentGiven, internalCandidateId, tags, applications, pools, candidateConsents, campaignTypes, attachments, documents, referrals, SocialNetworks"

    headers = {
        "content-type": "application/json",
        "username": parameters.username,
        "password": parameters.password,
    }

    filter_criteria = ""
    if parameters.candidateFilter:
        filter_criteria = "where: {"
        for key, value in parameters.candidateFilter.dict().items():
            if value is not None:
                filter_criteria += f"{key}: {value}, "
        filter_criteria = filter_criteria[:-2] + "}"

    query = f"{{ candidates(first: {parameters.first}, after: {parameters.after}, sortBy: {parameters.sortBy}, orderBy: {parameters.orderBy.value}{filter_criteria}) {{{candidateFullDto}}} }}"

    response = requests.get(
        TALENTLINK_CANDIDATES_ENDPOINT
        + "?query="
        + query
        + "&api_key="
        + parameters.api_key
        + "&stream=true",
        headers=headers,
    )
    if response.status_code != 200:
        adapter.error(
            "Failed to fetch profiles. Status code: %s, Response: %s",
            response.status_code,
            response.text,
        )
    profiles = []
    for chunk in response.iter_lines():
        adapter.info("Pulling profiles from Talentlink, chunk: %s", chunk)
        if chunk:
            data = json.loads(chunk)
            if "data" in data:
                candidates = data["data"]["candidates"]
                profiles.extend(candidates)

    for profile in profiles:
        yield profile


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    headers = {
        "content-type": "application/json",
        "username": parameters.username,
        "password": parameters.password,
    }

    url = f"{TALENTLINK_CANDIDATES_ENDPOINT}?api_key=" + parameters.api_key

    failed_profiles = []

    for profile in profiles:
        response = requests.post(url, headers=headers, data=profile)

        if response.status_code == 201:
            adapter.info("Candidate profile pushed successfully.")
        else:
            adapter.error(
                "Failed to push candidate profile. Status code: %s, Response: %s",
                response.status_code,
                response.text,
            )
            failed_profiles.append(profile)

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    """
    - Username: CompanyName:username:BO (i. e.MyCompany:JohnDoe:BO)
    - Password - same as the user password for Talentlink
    """

    # definition of fields available in the response
    PublishedAdvertDTO = "id, status, postingStartDate, postingEndDate, creationDate, updateDate, postingUser, navigation, jobboardConfiguration, advertId, jobAdTitle, comment, recruitingCompany, location, defaultJobLocation, applicationProcess, showCompensation, showRecruiter, keyword, language, advertCreationDate, advertExpirationDate, advertUpdateDate, assignedImages, attachments, strapline, opening, customFields, lovs, freeFormFields, actions, jobLocations, technicalId, structuredData, categories, onSite, hybrid, remote,"

    headers = {
        "content-type": "application/json",
        "username": f"{parameters.companyname}:{parameters.username}:BO",
        "password": parameters.password,
    }

    filter_criteria = ""
    if parameters.publishedAdvertsFilter:
        filter_criteria = "where: {"
        for key, value in parameters.publishedAdvertsFilter.dict().items():
            if value is not None:
                filter_criteria += f"{key}: {value}, "
        filter_criteria = filter_criteria[:-2] + "}"

    query = f"{{ publishedAdverts(first: {parameters.first}, after: {parameters.after}, sortBy: {parameters.sortBy}, orderBy: {parameters.orderBy.value}{filter_criteria}) {{{PublishedAdvertDTO}}} }}"

    response = requests.get(
        TALENTLINK_JOBS_ENDPOINT
        + "?query="
        + query
        + "&api_key="
        + parameters.api_key
        + "&stream=true",
        headers=headers,
    )
    if response.status_code != 200:
        adapter.error(
            "Failed to fetch jobs. Status code: %s, Response: %s",
            response.status_code,
            response.text,
        )

    jobs = []
    for chunk in response.iter_lines():
        adapter.info("Pulling jobs from Talentlink, chunk: %s", chunk)
        if chunk:
            data = json.loads(chunk)
            if "data" in data:
                published_adverts = data["data"]["publishedAdverts"]
                jobs.extend(published_adverts)

    for job in jobs:
        yield job


def read_applications(
    adapter: LoggerAdapter,
    parameters: ReadApplicationsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:

    # definition of fields available in the response
    ApplicationDTO = "id, sourceChannelName, sourceChannelType, sourcingMedium, creation, update, applicationDate, status, memo, shortListed, archived, activeApplication, hasContracts, completionReason, statusComment, candidate, opening, documents, applicationFollowups, candidateConsents, score, attachments, "

    headers = {
        "content-type": "application/json",
        "username": parameters.username,
        "password": parameters.password,
    }

    filter_criteria = ""
    if parameters.applicationFilter:
        filter_criteria = "where: {"
        for key, value in parameters.applicationFilter.dict().items():
            if value is not None:
                filter_criteria += f"{key}: {value}, "
        filter_criteria = filter_criteria[:-2] + "}"

    query = f"{{ applications(first: {parameters.first}, after: {parameters.after}, sortBy: {parameters.sortBy}, orderBy: {parameters.orderBy.value}{filter_criteria}) {{{ApplicationDTO}}} }}"

    response = requests.get(
        TALENTLINK_APPLICATIONS_ENDPOINT
        + "?query="
        + query
        + "&api_key="
        + parameters.api_key
        + "&stream=true",
        headers=headers,
    )
    if response.status_code != 200:
        adapter.error(
            "Failed to fetch applications. Status code: %s, Response: %s",
            response.status_code,
            response.text,
        )

    applications = []
    for chunk in response.iter_lines():
        adapter.info("Pulling applications from Talentlink, chunk: %s", chunk)
        if chunk:
            data = json.loads(chunk)
            if "data" in data:
                applications_data = data["data"]["applications"]
                applications.extend(applications_data)

    for application in applications:
        yield application


TalentLinkProfileWarehouse = Warehouse(
    name="talentlink_profiles",
    data_schema=TalentLinkProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfileParameters,
        function=read_profiles,
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_profiles,
    ),
)

TalentLinkJobWarehouse = Warehouse(
    name="talentlink_jobs",
    data_schema=TalentLinkJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)

TalentLinkApplicationWarehouse = Warehouse(
    name="talentlink_applications",
    data_schema=TalentLinkApplication,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadApplicationsParameters,
        function=read_applications,
    ),
)

TalentLinkScoreWarehouse = Warehouse(
    name="talentlink_scores",
    data_schema=TalentLinkScore,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadScoreParameters,
        function=read_scores,
    ),
)
