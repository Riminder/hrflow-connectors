import datetime
import typing as t

from pydantic import BaseModel, Field, conbytes, validator

from hrflow_connectors.connectors.ukgpro.utils.enums import (
    UKGProDocumentType,
    UKGProFileType,
    UKGProGrantType,
    UKGProRecruitingScope,
)
from hrflow_connectors.connectors.ukgpro.utils.errors import UKGProEmptyString
from hrflow_connectors.core import FieldType

_UKGPRO_ATTACHMENT_MAX_SIZE_MB = 6


class UKGProAuthentication(BaseModel):
    identity_server: str = Field(
        ...,
        description="The identity server host name. Included in the request URL.",
        field_type=FieldType.Other,
    )
    tenant: str = Field(
        ...,
        description=(
            "A customer's tenant name. Provided by UKG. Included in the request URL."
        ),
        field_type=FieldType.Other,
    )
    client_id: str = Field(
        ...,
        description=(
            "A client ID that allows to access data of the specific customer. Assigned"
            " by UKG. Included in the request data."
        ),
        field_type=FieldType.Auth,
        repr=False,
    )
    client_secret: str = Field(
        ...,
        description=(
            "A client secret associated with the client ID. Assigned by UKG. Included"
            " in the request data."
        ),
        field_type=FieldType.Auth,
        repr=False,
    )
    grant_type: UKGProGrantType = Field(
        UKGProGrantType.CLIENT_CREDENTIALS,
        description="Always set to “client_credentials”. Included in the request data.",
        field_type=FieldType.Other,
    )
    scope: t.List[UKGProRecruitingScope] = Field(
        [
            UKGProRecruitingScope.CANDIDATE_CREATE,
            UKGProRecruitingScope.CANDIDATE_READ,
            UKGProRecruitingScope.APPLICATION_CREATE,
            UKGProRecruitingScope.APPLICATION_READ,
        ],
        description=(
            "Always set to “recruiting.domain.candidate-import.create"
            " recruiting.domain.candidate-import.read"
            " recruiting.domain.application-import.create"
            " recruiting.domain.application-import.read”. Included in the request data."
        ),
        field_type=FieldType.Other,
    )


class UKGProDomain(BaseModel):
    environment: str
    tenant: str


class UKGProCreator(BaseModel):
    id: t.Optional[str] = None


class UKGProName(BaseModel):
    first: str = Field(
        ...,
        description="Candidate's first name. Maximum 100 characters.",
        max_length=100,
    )
    middle: t.Optional[str] = Field(
        None,
        description="Candidate's middle name. Maximum 50 characters.",
        max_length=50,
    )
    last: str = Field(
        ...,
        description="Candidate's last name. Maximum 100 characters.",
        max_length=100,
    )
    former: t.Optional[str] = Field(
        None,
        description="Candidate's former name. Maximum 100 characters.",
        max_length=100,
    )
    prefix: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's name prefix ID. Maximum 50 characters. Use the GET prefixes"
            " endpoint to retrieve prefix IDs."
        ),
        max_length=50,
    )
    suffix: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's name suffix ID. Maximum 50 characters. Use the GET suffixes"
            " endpoint to retrieve suffix IDs."
        ),
        max_length=50,
    )

    class Config:
        validate_assignment = True


class UKGProState(BaseModel):
    code: str = Field(
        ...,
        description="Candidate's state code. Maximum 6 characters.",
        max_length=6,
    )

    class Config:
        validate_assignment = True


class UKGProCountry(BaseModel):
    code: t.Optional[str] = Field(
        ..., description="Candidate's country code. No Maximum Length validation."
    )


class UKGProAddress(BaseModel):
    line1: t.Optional[str] = Field(
        ...,
        description="Candidate's address line 1. Maximum 255 characters.",
        max_length=255,
    )
    line2: t.Optional[str] = Field(
        None,
        description="Candidate's address line 2. Maximum 255 characters.",
        max_length=255,
    )
    city: t.Optional[str] = Field(
        None,
        description="Candidate's address city. Maximum 255 characters.",
        max_length=255,
    )
    state: t.Optional[UKGProState] = None
    postal_code: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's postal code. Minimum 3 characters, maximum 50 characters."
        ),
        min_length=3,
        max_length=50,
    )
    country: t.Optional[UKGProCountry] = None

    class Config:
        validate_assignment = True


class UKGProPhone(BaseModel):
    primary: t.Optional[str] = Field(
        None,
        description="Candidate's primary phone number. Maximum 25 characters.",
        max_length=25,
    )
    secondary: t.Optional[str] = Field(
        None,
        description="Candidate's secondary phone number. Maximum 25 characters.",
        max_length=25,
    )

    class Config:
        validate_assignment = True


class UKGProContactInfo(BaseModel):
    email: str = Field(
        ...,
        description=(
            "Candidate's email address. This will also be the username they can use to"
            " login. Must be a valid email address. Maximum 254 characters."
        ),
        max_length=254,
    )
    phone: t.Optional[UKGProPhone] = None
    address: t.Optional[UKGProAddress] = None

    class Config:
        validate_assignment = True


class UKGProHyperlink(BaseModel):
    name: str = Field(
        ...,
        description=(
            "The name of the hyperlink. Must not be an empty string. Maximum 250"
            " characters."
        ),
        min_length=1,
        max_length=250,
    )
    url: str = Field(
        ...,
        description=(
            "The URL of the hyperlink. This must be a valid url (e.g."
            " https://www.google.com)"
        ),
    )

    class Config:
        validate_assignment = True


class UKGProWorkExperienceFrom(BaseModel):
    month: t.Optional[int] = Field(
        None,
        description=(
            "Month number of the start date for the work experience item. The value"
            " must be a number from 1 through 12."
        ),
        ge=1,
        le=12,
    )
    year: t.Optional[int] = Field(
        None,
        description=(
            "Year number of the start date for the work experience item. The value"
            " cannot be more than 100 years in the past at the time of the API request."
        ),
        ge=datetime.datetime.now().year - 100,
    )

    class Config:
        validate_assignment = True


class UKGProWorkExperienceTo(BaseModel):
    month: t.Optional[int] = Field(
        None,
        description=(
            "Month number of the end date for the work experience item. The value must"
            " be a number from 1 through 12."
        ),
        ge=1,
        le=12,
    )
    year: t.Optional[int] = Field(
        None,
        description=(
            "Year number of the end date for the work experience item. The value cannot"
            " be more than 5 years in the future from the time of the API request."
        ),
        le=datetime.datetime.now().year + 5,
    )

    class Config:
        validate_assignment = True


class UKGProEducationFrom(BaseModel):
    month: t.Optional[int] = Field(
        None,
        description=(
            "Month number of the start date of the education item. The value must be a"
            " number from 1 through 12."
        ),
        ge=1,
        le=12,
    )
    year: t.Optional[int] = Field(
        None,
        description=(
            "Year of the start date of the education item. The value cannot be more"
            " than 100 years in the past at the time of the API request. The value"
            " cannot be greater than the education.to.year value for this education"
            " item."
        ),
        ge=datetime.datetime.now().year - 100,
    )

    class Config:
        validate_assignment = True


class UKGProEducationTo(BaseModel):
    month: t.Optional[int] = Field(
        None,
        description=(
            "Month number of the end date of the education item. The value must be a"
            " number from 1 through 12."
        ),
        ge=1,
        le=12,
    )
    year: t.Optional[int] = Field(
        None,
        description=(
            "Year of the end date of the education item. At the time of the API"
            " request, the value cannot be more than 100 years in the past or more than"
            " 5 years in the future."
        ),
        ge=datetime.datetime.now().year - 100,
        le=datetime.datetime.now().year + 5,
    )


class UKGProWorkExperience(BaseModel):
    job_title: str = Field(
        ...,
        description="Job title of the work experience item. Max 100 characters.",
        max_length=100,
    )
    company: str = Field(
        ...,
        description=(
            "The name of the company where the candidate worked in the work experience"
            " item."
        ),
    )
    location: t.Optional[str] = Field(
        None,
        description=(
            "The location of the candidate's job in the work experience item. Maximum"
            " 20 characters."
        ),
        max_length=20,
    )
    from_: t.Optional[UKGProWorkExperienceFrom] = Field(None, alias="from")
    to: t.Optional[UKGProWorkExperienceTo] = None
    description: t.Optional[str] = Field(
        None,
        description="Description of the work experience item. Maximum 2000 characters.",
        max_length=2000,
    )

    class Config:
        validate_assignment = True


class UKGProDegree(BaseModel):
    name: str = Field(
        ..., description="The name of the degree associated with the education item."
    )
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of the degree associated with the education item. The Candidate"
            " Import API currently requires this value to be null, because the endpoint"
            " to read degree IDs is not available."
        ),
    )


class UKGProSchool(BaseModel):
    name: str = Field(
        ...,
        description=(
            "The name of the school associated with the education item. Maximum 100"
            " characters."
        ),
        max_length=100,
    )
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of the school associated with the education item. The Candidate"
            " Import API currently requires this value to be null, because the endpoint"
            " to read school IDs is not available."
        ),
    )

    class Config:
        validate_assignment = True


class UKGProMajor(BaseModel):
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of the major field of study associated with the education item. Use"
            " the GET fields-of-study API to retrieve the IDs of available fields of"
            " study."
        ),
    )


class UKGProMinor(BaseModel):
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of the minor field of study associated with the education item. Use"
            " the GET fields-of-study API to retrieve the IDs of available fields of"
            " study."
        ),
    )


class UKGProEducation(BaseModel):
    degree: t.Optional[UKGProDegree] = Field(
        None, description="Array containing education objects."
    )
    school: UKGProSchool
    major: t.Optional[UKGProMajor] = None
    minor: t.Optional[UKGProMinor] = None
    from_: t.Optional[UKGProEducationFrom] = Field(None, alias="from")
    to: t.Optional[UKGProEducationTo] = None
    description: t.Optional[str] = Field(
        None,
        description="Decription of the education item. Maximum 1500 characters.",
        max_length=1500,
    )

    class Config:
        validate_assignment = True


class UKGProLicenseNameId(BaseModel):
    name: str = Field(
        ...,
        description="Name of the license or certification. Maximum 250 characters.",
        max_length=250,
    )
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of an existing license to add to the candidate record. The"
            " Candidate Import API currently requires this value to be null, because"
            " the endpoint to read license IDs is not available."
        ),
    )

    class Config:
        validate_assignment = True


class UKGProMotivation(BaseModel):
    id: str = Field(
        ...,
        description=(
            "ID of a Motivation item in the UKG Pro Recruiting system. The Motivation"
            " must be existing, not archived, and unique for the candidate. The"
            " Candidate Import API currently requires this value to be null, because"
            " the endpoint to read Motivation IDs is not available."
        ),
    )


class UKGProBehaviour(BaseModel):
    id: str = Field(
        ...,
        description=(
            "ID of a Behavior item in the UKG Pro Recruiting system. The Behavior must"
            " be existing, not archived, and unique for the candidate. The Candidate"
            " Import API currently requires this value to be null, because the endpoint"
            " to read Behavior IDs is not available."
        ),
    )


class UKGProComment(BaseModel):
    author_id: str = Field(
        ...,
        description=(
            "ID of the internal candidate (employee) who is the author of the comment."
        ),
    )
    comment: str = Field(
        ...,
        description=(
            "Text of the comment. The value cannot be empty or contain only space."
            " Maximum 4000 characters."
        ),
        max_length=4000,
    )
    is_important: t.Optional[bool] = Field(
        None,
        description=(
            "Indicates whether to apply the Important flag to the comment. Can only be"
            " specified when comment is provided."
        ),
    )
    created_at: t.Optional[str] = Field(
        None,
        description=(
            "The date of the comment. The value cannot be in the future at the time of"
            " the API call."
        ),
        min_length=16,
    )

    class Config:
        validate_assignment = True

    @validator("comment")
    @classmethod
    def _valid_comment(cls, value: str) -> str:
        if len(value.replace(" ", "")) == 0:
            raise UKGProEmptyString
        return value

    @validator("created_at")
    @classmethod
    def _valid_created_at(cls, value: str) -> str:
        try:
            dt = datetime.datetime.fromisoformat(value[:16])
            n = datetime.datetime.now()
            return value if dt <= n else None
        except Exception:
            return None


class UKGProLicense(BaseModel):
    license: UKGProLicenseNameId
    number: t.Optional[str] = Field(
        None,
        description=(
            "License number of the license or certification. Maximum 20 characters."
        ),
        max_length=20,
    )
    achieved_date: t.Optional[str] = Field(
        None,
        description=(
            "Date of completion of the license or certification. At the time of the API"
            " request, this value cannot be more than 100 years in the past or 5 years"
            " in the future. The value cannot be later than the renewal_date."
        ),
        min_length=16,
    )
    renewal_date: t.Optional[str] = Field(
        None,
        description=(
            "Renewal date of the license or certification. This value cannot be earlier"
            " than the achieved_date."
        ),
        min_length=16,
    )

    class Config:
        validate_assignment = True

    @validator("achieved_date")
    @classmethod
    def _valid_achieved_date(cls, value: str) -> str:
        try:
            dt = datetime.datetime.fromisoformat(value[:16])
            n = datetime.datetime.now()
            if dt.year >= n.year - 100 and dt.year <= n.year + 5:
                return value
        except Exception:
            return None

    @validator("renewal_date")
    @classmethod
    def _valid_renewal_date(cls, value: str) -> str:
        try:
            datetime.datetime.fromisoformat(value[:16])
            return value
        except Exception:
            return None


class UKGProSkillNameId(BaseModel):
    name: str = Field(
        ...,
        description=(
            "The name of the skill to add to the candidate record. If included: Minimum"
            " 1 character, maximum 80 characters."
        ),
        min_length=1,
        max_length=80,
    )
    id: t.Optional[str] = Field(
        None,
        description=(
            "The ID of an existing skill in the UKG Pro Recruiting system to be added"
            " to the candidate record. The Candidate Import API currently requires this"
            " value to be null, because the endpoint to read skill IDs is not"
            " available."
        ),
    )


class UKGProSkillProficiencyLevel(BaseModel):
    id: str = Field(
        ...,
        description=(
            "The ID of the proficiency level that applies to the candidate's skill. Use"
            " the GET proficiency-levels endpoint to retrieve proficiency level IDs."
        ),
    )
    name: t.Optional[str] = Field(
        None,
        description=(
            "Name of the proficiency level that applies to the candidate's skill."
        ),
    )
    ordinal: t.Optional[int] = Field(
        None,
        description=(
            "The ordinal number of the proficiency level that applies to the"
            " candidate's skill."
        ),
    )


class UKGProSkill(BaseModel):
    skill: UKGProSkillNameId
    proficiency_level: UKGProSkillProficiencyLevel


class UKGProProvider(BaseModel):
    id: t.Optional[str] = Field(
        None,
        description=(
            "ID of provider that generated the candidate. For the Candidate Import API,"
            " use: null"
        ),
    )
    name: str = Field(
        "HrFlow.ai",
        description=(
            "The name of the provider who created the candidate. Maximum 100"
            " characters."
        ),
        max_length=100,
    )
    type: t.Optional[str] = Field(
        "ThirdPartyIntegration",
        description="For the Candidate Import API, use: ThirdPartyIntegration",
    )
    method: t.Optional[str] = Field(
        "CandidateImport",
        description="For the Candidate Import API, use: CandidateImport",
    )


class UKGProCandidate(BaseModel):
    created_at: t.Optional[str] = Field(
        None,
        description=(
            "Date the candidate was created; cannot be a future date. If not provided,"
            " it will default to the date and time when the API call is executed."
        ),
        min_length=16,
    )
    creator: t.Optional[UKGProCreator] = Field(
        None,
        description=(
            "Creator of this candidate record; the creator must exist in the system."
        ),
    )
    password: t.Optional[str] = Field(
        "", description="Must be empty for the Candidate Import API."
    )
    is_internal: t.Optional[bool] = Field(
        False, description="Indicates whether the candidate is an internal employee."
    )
    is_active: t.Optional[bool] = Field(
        None,
        description=(
            "Indicates whether the candidate is an active employee. Can only be"
            " specified when is_internal is true."
        ),
    )
    name: UKGProName
    contact_info: UKGProContactInfo
    phone: t.Optional[UKGProPhone] = None
    address: t.Optional[UKGProAddress] = None
    hyperlinks: t.Optional[t.List[UKGProHyperlink]] = Field(
        None, description="Array containing hyperlink objects."
    )
    workexperience: t.Optional[t.List[UKGProWorkExperience]] = Field(
        None, description="Array containing work experience objects."
    )
    education: t.Optional[t.List[UKGProEducation]] = Field(
        None, description="Array containing education objects."
    )
    licenses: t.Optional[t.List[UKGProLicense]] = Field(
        None, description="Array containing license objects."
    )
    behaviours: t.Optional[t.List[UKGProBehaviour]] = Field(
        None, description="Array containing behavior objects."
    )
    motivations: t.Optional[t.List[UKGProMotivation]] = Field(
        None, description="Array containing motivation objects."
    )
    comments: t.Optional[t.List[UKGProComment]] = Field(
        None, description="Array containing comment objects."
    )
    skills: t.Optional[t.List[UKGProSkill]] = Field(
        None, description="Array containing skill objects."
    )
    integration_user_id: t.Optional[str] = Field(
        None,
        description=(
            "Valid only for internal candidates. Only alphanumeric characters and"
            " hyphens are allowed. Maximum 50 characters."
        ),
        regex="^[0-9A-Za-z-]{,50}$",
        max_length=50,
    )
    provider: t.Optional[UKGProProvider] = Field(
        UKGProProvider(), description="Object containing provider data."
    )

    class Config:
        validate_assignment = True

    @validator("created_at")
    @classmethod
    def _valid_created_at(cls, value: str) -> str:
        try:
            dt = datetime.datetime.fromisoformat(value[:16])
            n = datetime.datetime.now()
            return value if dt <= n else None
        except Exception:
            return None


class UKGProAttachDocument(BaseModel):
    file_name: str = Field(
        ...,
        description=(
            "The file name of the document attachment, including the file extension"
            " (for example: file.doc). The file name cannot be empty, contain only"
            ' space, or contain any of the following characters: / \\ ? : * " < > |.'
            "The file extension cannot be empty, contain only space, or contain any"
            " non-supported file type."
        ),
        regex=(
            "^[^/\\?:*<>]+\\.("
            f"{'|'.join(map(lambda s: s.lower(), UKGProFileType._member_map_))})$"
        ),
    )
    file_data: conbytes(max_length=_UKGPRO_ATTACHMENT_MAX_SIZE_MB * 10**6) = Field(
        ...,
        description=(
            "The file data of the document attachment. The maximum file size of a"
            f" document attachment is {_UKGPRO_ATTACHMENT_MAX_SIZE_MB}MB."
        ),
    )
    document_type: UKGProDocumentType = Field(
        ...,
        description="The type or purpose of the document attachment.",
    )
    description: t.Optional[str] = Field(
        None,
        description="Description of the document attachment. Maximum 50 characters.",
        max_length=50,
    )

    class Config:
        validate_assignment = True
