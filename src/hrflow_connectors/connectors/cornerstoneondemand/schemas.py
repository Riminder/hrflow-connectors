import typing as t

from pydantic import BaseModel, Field, conbytes

from hrflow_connectors.connectors.cornerstoneondemand.utils.enums import (
    CornerstoneOnDemandQuestionType,
    CornerstoneOnDemandSupportedISOLanguageCode,
)
from hrflow_connectors.core import FieldType

_CORNERSTONE_ONDEMAND_ADDRESS1_MAX_LENGTH = 110
_CORNERSTONE_ONDEMAND_ADDRESS2_MAX_LENGTH = 55
_CORNERSTONE_ONDEMAND_CITY_MAX_LENGTH = 35
_CORNERSTONE_ONDEMAND_COUNTRY_MAX_LENGTH = 2
_CORNERSTONE_ONDEMAND_EMAIL_MAX_LENGTH = 128
_CORNERSTONE_ONDEMAND_FILE_NAME_MAX_LENGTH = 50
_CORNERSTONE_ONDEMAND_FIRST_NAME_MAX_LENGTH = 200
_CORNERSTONE_ONDEMAND_LAST_NAME_MAX_LENGTH = 200
_CORNERSTONE_ONDEMAND_POSTAL_CODE_MAX_LENGTH = 20
_CORNERSTONE_ONDEMAND_PHONE_NUMBER_MAX_LENGTH = 20
_CORNERSTONE_ONDEMAND_STATE_MAX_LENGTH = 30
_CORNERSTONE_ONDEMAND_SUBMISSION_SOURCE_MAX_LENGTH = 50
_CORNERSTONE_ONDEMAND_SUBMISSION_SOURCE_NAME_MAX_LENGTH = 250

_CORNERSTONE_ONDEMAND_ATTACHMENT_MAX_MB_SIZE = 15
_CORNERSTONE_ONDEMAND_RESUME_MAX_MB_SIZE = 10
_CORNERSTONE_ONDEMAND_COVER_LETTER_MAX_MB_SIZE = 5
_CORNERSTONE_ONDEMAND_ACCEPTED_COVER_LETTER_EXTENSIONS = ["txt", "doc", "docx", "pdf"]
_CORNERSTONE_ONDEMAND_ACCEPTED_RESUME_EXTENSIONS = ["txt", "doc", "docx", "pdf"]
_CORNERSTONE_ONDEMAND_ACCEPTED_ATTACHMENTS_EXTENSIONS = [
    "txt",
    "doc",
    "docx",
    "pdf",
    "bmp",
    "gif",
    "jpg",
    "jpeg",
    "png",
]


def _file_name_regex_get(exts: t.List[str]) -> str:
    """
    Creates a regex matching the possible file names for the given extensions.

    Args:
      exts (list[str]): list of allowed extensions

    Returns:
      String representation of the regex.

    Example:
    >>> _file_name_regex_get(["pdf", "jpeg"])
    '^.+\\.(pdf|jpeg)$'
    """

    return f'^.+\\.({"|".join(exts)})$'


class CornerstoneOnDemandCoverLetter(BaseModel):
    fileName: str = Field(
        ...,
        description=(
            "Name of the cover letter file. This should also include the file"
            " extension."
        ),
        regex=_file_name_regex_get(
            _CORNERSTONE_ONDEMAND_ACCEPTED_COVER_LETTER_EXTENSIONS
        ),
        max_length=_CORNERSTONE_ONDEMAND_FILE_NAME_MAX_LENGTH,
    )
    file: conbytes(
        max_length=_CORNERSTONE_ONDEMAND_COVER_LETTER_MAX_MB_SIZE * 10**6
    ) = Field(
        ...,
        description=(
            "The raw content of the candidate's resume file. Maximum size is"
            f" {_CORNERSTONE_ONDEMAND_COVER_LETTER_MAX_MB_SIZE}MB."
        ),
    )
    text: str = Field(
        "",
        description=(
            "Text of an applicant's cover letter. This open text option is only"
            " supported for cover letters."
        ),
    )

    class Config:
        validate_assignment = True


class CornerstoneOnDemandQuestion(BaseModel):
    id: str = Field(
        ..., description="This value will be the internal id for each question type."
    )
    response: t.Optional[str]
    type: CornerstoneOnDemandQuestionType = Field(
        ...,
        description=(
            "Type of question. This is an enum of: Disclaimer, Compliance,"
            " Prescreening."
        ),
    )


class CornerstoneOnDemandContactDetails(BaseModel):
    PhoneNumber: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's phone number. All characters are supported, but will validate"
            " against HTML content. Clients may configure a phone number to be required"
            " or optional in Admin Settings. CornerstoneOnDemand validates against this"
            " field, because it may be used to merge duplicate candidates in the"
            " system. To mark this field required or optional, visit Admin > Tools >"
            " Recruit > General Recruitment: Requisition and Applicant Preferences >"
            " Applicant Preferences: Phone Required."
        ),
        max_length=_CORNERSTONE_ONDEMAND_PHONE_NUMBER_MAX_LENGTH,
    )
    address1: t.Optional[str] = Field(
        None,
        description="Candidate's address.",
        max_length=_CORNERSTONE_ONDEMAND_ADDRESS1_MAX_LENGTH,
    )
    address2: t.Optional[str] = Field(
        None,
        description="Candidate's address.",
        max_length=_CORNERSTONE_ONDEMAND_ADDRESS2_MAX_LENGTH,
    )
    city: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's city. This is free form. There are no restrictions on the text"
            " field irrespective of country. Validations will only be made against HTML"
            " content."
        ),
        max_length=_CORNERSTONE_ONDEMAND_CITY_MAX_LENGTH,
    )
    country: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's country which should be formatted in ISO-3166 alpha-2 code for"
            " example, US for United States. The required number of characters is 2."
        ),
        min_length=_CORNERSTONE_ONDEMAND_COUNTRY_MAX_LENGTH,
        max_length=_CORNERSTONE_ONDEMAND_COUNTRY_MAX_LENGTH,
    )
    postalCode: t.Optional[str] = Field(
        None,
        description="Candidate's postal code.",
        max_length=_CORNERSTONE_ONDEMAND_POSTAL_CODE_MAX_LENGTH,
    )
    state: t.Optional[str] = Field(
        None,
        description=(
            "Candidate's state. There are no restrictions on the text field."
            " Validations will only be made against HTML content."
        ),
        max_length=_CORNERSTONE_ONDEMAND_STATE_MAX_LENGTH,
    )

    class Config:
        validate_assignment = True


class CornerstoneOnDemandCandidate(BaseModel):
    contactDetails: t.Optional[CornerstoneOnDemandContactDetails] = Field(
        None, description="Data about the candidate's contact details."
    )
    email: str = Field(
        ...,
        description="Candidate's email.",
        max_length=_CORNERSTONE_ONDEMAND_EMAIL_MAX_LENGTH,
    )
    firstName: str = Field(
        ...,
        description="Candidate's first name.",
        max_length=_CORNERSTONE_ONDEMAND_FIRST_NAME_MAX_LENGTH,
    )
    language: t.Optional[CornerstoneOnDemandSupportedISOLanguageCode] = Field(
        None,
        description=(
            "The culture name of the candidate, en-US. This will default to the"
            " portal's default culture when not passed in."
        ),
    )
    lastName: str = Field(
        ...,
        description="Candidate's last name.",
        max_length=_CORNERSTONE_ONDEMAND_LAST_NAME_MAX_LENGTH,
    )

    class Config:
        validate_assignment = True


class CornerstoneOnDemandApplicationPreferences(BaseModel):
    sendEmail: bool = Field(
        True,
        description=(
            "This is an optional field allowing an API consumer to specify if the"
            " candidate should or should not receive an email upon successful"
            " submission of their application. When set sendEmail is true, the"
            " applicant will receive the apply as guest, or standard application"
            " submission email. This will be dependent on whether the candidate's"
            " profile has or has not already been claimed. Some clients also require"
            " applicants to claim their email address before successfully claiming"
            " their account. This workflow will remain the same. If left empty, this"
            " property will default to true."
        ),
    )


class CornerstoneOnDemandAdditionalAttachment(BaseModel):
    fileName: str = Field(
        ...,
        description=(
            "Name of the candidate's additional attachment file. This should also"
            " include the file extension."
        ),
        regex=_file_name_regex_get(
            _CORNERSTONE_ONDEMAND_ACCEPTED_ATTACHMENTS_EXTENSIONS
        ),
        max_length=_CORNERSTONE_ONDEMAND_FILE_NAME_MAX_LENGTH,
    )
    file: conbytes(
        max_length=_CORNERSTONE_ONDEMAND_RESUME_MAX_MB_SIZE * 10**6
    ) = Field(
        ...,
        description=(
            "The raw content of the candidate's resume file. Maximum size"
            " {_CORNERSTONE_ONDEMAND_RESUME_MAX_MB_SIZE}MB."
        ),
    )
    id: str = Field(..., description="The id of the additional attachment.")

    class Config:
        validate_assignment = True


class CornerstoneOnDemandCandidatePreferences(BaseModel):
    futureOpportunityOptIn: t.Optional[bool] = Field(
        None, description="Candidate's decision to be considered for other positions."
    )


class CornerstoneOnDemandResume(BaseModel):
    fileName: str = Field(
        ...,
        description=(
            "Name of the resume file. This should also include the file extension."
        ),
        regex=_file_name_regex_get(_CORNERSTONE_ONDEMAND_ACCEPTED_RESUME_EXTENSIONS),
        max_length=_CORNERSTONE_ONDEMAND_FILE_NAME_MAX_LENGTH,
    )
    file: conbytes(
        max_length=_CORNERSTONE_ONDEMAND_RESUME_MAX_MB_SIZE * 10**6
    ) = Field(
        ...,
        description=(
            "The raw content of the candidate's resume file. Maximum size is"
            f" {_CORNERSTONE_ONDEMAND_RESUME_MAX_MB_SIZE}MB"
        ),
    )

    class Config:
        validate_assignment = True


class CornerstoneOnDemandSource(BaseModel):
    submissionSource: t.Optional[str] = Field(
        None,
        description=(
            "How the application was submitted. If a custom submission source does not"
            " exist in CSOD, it will be added automatically to Admin settings. If the"
            ' submission source is left empty, the source name, "Candidate API" will be'
            " used."
        ),
        max_length=_CORNERSTONE_ONDEMAND_SUBMISSION_SOURCE_MAX_LENGTH,
    )
    submissionSourceName: t.Optional[str] = Field(
        None,
        description=(
            "A user-friendly display name showing how the application was submitted. If"
            " not specified, this will be the same as submissionSource."
        ),
        max_length=_CORNERSTONE_ONDEMAND_SUBMISSION_SOURCE_NAME_MAX_LENGTH,
    )

    class Config:
        validate_assignment = True


class CornerstoneOnDemandAuthentication(BaseModel):
    client_id: str = Field(
        ...,
        description='The id of the client. An example would be "dbq2kjiql2c4".',
        field_type=FieldType.Auth,
        repr=False,
    )
    client_secret: str = Field(
        ...,
        description=(
            "The secret of the client. An example would be"
            ' "l4nqwza+7RbK0rrzs16VMeH+5dWEsFjsRSXtQ0MwL+TSSWvZGliUkgUfIenAk0+1Yx0yPtTs'
            '+bSmlotR2KCVGA==".'
        ),
        field_type=FieldType.Auth,
        repr=False,
    )
    corpname: str


class CornerstoneOnDemandCreateCandidate(BaseModel):
    additionalAttachments: t.Optional[
        t.List[CornerstoneOnDemandAdditionalAttachment]
    ] = Field(
        None,
        description="A collection of the candidate's additional attachments.",
        field_type=FieldType.Other,
    )
    applicationPreferences: t.Optional[CornerstoneOnDemandApplicationPreferences]
    candidate: CornerstoneOnDemandCandidate = Field(
        None,
        description=(
            "Data about the candidate who is being considered for a specific"
            " requisition."
        ),
        field_type=FieldType.Other,
    )
    candidatePreferences: t.Optional[CornerstoneOnDemandCandidatePreferences] = Field(
        None, field_type=FieldType.Other
    )
    coverLetter: t.Optional[CornerstoneOnDemandCoverLetter] = Field(
        None, field_type=FieldType.Other
    )
    jobRequisitionId: str = Field(
        "",  # this allows to avoid a pydantic error when formatting
        description=(
            'The ATS job requisition\'s identifier. This is a "ref" value and not the'
            ' internal "id". A correct example is Req123.'
        ),
        field_type=FieldType.Other,
    )
    questions: t.Optional[t.List[CornerstoneOnDemandQuestion]] = Field(
        None,
        description=(
            "A collection of application submission data for questions of type:"
            " Disclaimer, Compliance and Prescreening."
        ),
        field_type=FieldType.Other,
    )
    resume: t.Optional[CornerstoneOnDemandResume] = Field(
        None, field_type=FieldType.Other
    )
    source: t.Optional[CornerstoneOnDemandSource] = Field(
        None, field_type=FieldType.Other
    )
