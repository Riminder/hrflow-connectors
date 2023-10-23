import typing as t

from pydantic import BaseModel, Field

from hrflow_connectors.connectors.oracleorc.utils.enums import (
    OracleORCLinkKind,
    OracleORCLinkRelation,
)
from hrflow_connectors.core import FieldType


class OracleORCAuth(BaseModel):
    username: str = Field(..., repr=False, field_type=FieldType.Auth)
    password: str = Field(..., repr=False, field_type=FieldType.Auth)


class OracleORCHostAndPort(BaseModel):
    host: str = Field(..., field_type=FieldType.Other)
    port: str = Field(..., field_type=FieldType.Other)


class OracleORCAttachment(BaseModel):
    ErrorStatusMessage: str = Field(
        None, description="The error message, if any, for the attachment."
    )
    Description: t.Optional[str] = Field(
        None, description="The description of the attachment.", max_length=255
    )
    DatatypeCode: str = Field(
        "FILE", description="A value that indicates the data type.", max_length=30
    )
    FileName: t.Optional[str] = Field(
        None, description="The file name of the attachment.", max_length=2048
    )
    Title: t.Optional[str] = Field(None, description="The title of the attachment.")
    Uri: t.Optional[str] = Field(
        None, description="The URL of a web page type attachment.", max_length=4000
    )
    UploadedFileName: t.Optional[str] = Field(
        None, description="The name to assign to a new attachment file."
    )
    DmDocumentId: t.Optional[str] = Field(
        None,
        description="The document ID from which the attachment is created.",
        max_length=255,
    )
    FileUrl: t.Optional[str] = Field(None, description="The URI of the file.")
    ContentRepositoryFileShared: t.Optional[str] = Field(
        None, description="Indicates whether the attachment is shared."
    )
    AttachedDocumentId: int = Field(
        ..., description="The unique identifier of the attached document."
    )
    FileContents: t.Optional[bytes] = Field(
        None, description="The contents of the attachment."
    )
    UploadedFileContentType: t.Optional[str] = Field(
        None, description="The content type of the attachment."
    )
    ExpirationDate: t.Optional[str] = Field(
        None, description="The expiration date of the contents in the attachment."
    )
    ErrorStatusCode: t.Optional[str] = Field(
        None, description="The error code, if any, for the attachment."
    )
    DownloadInfo: t.Optional[str] = Field(
        None,
        description=(
            "JSON object represented as a string containing information used to"
            " programmatically retrieve a file attachment."
        ),
    )
    DmFolderPath: t.Optional[str] = Field(
        None,
        description="The folder path from which the attachment is created.",
        max_length=1000,
    )
    UploadedText: t.Optional[str] = Field(
        None, description="The text content for a new text attachment."
    )
    CategoryName: t.Optional[str] = Field(
        None, description="The category of the attachment.", max_length=30
    )
    UploadedFileLength: t.Optional[int] = Field(
        None, description="The size of the attachment file."
    )
    DmVersionNumber: t.Optional[str] = Field(
        None,
        description="The document version number from which the attachment is created.",
        max_length=255,
    )
    AsyncTrackerId: t.Optional[str] = Field(
        None,
        description=(
            "Attribute provided for the exclusive use by the Attachment UI components"
            " to assist in uploading files."
        ),
    )

    class Config:
        validate_assignment = True


class OracleORCLink(BaseModel):
    kind: OracleORCLinkKind = Field(
        ..., description="The kind of the related resource."
    )
    rel: OracleORCLinkRelation = Field(
        ...,
        description=(
            "The name of the relation to the resource instance.  Example: self."
        ),
    )
    name: str = Field(..., description="The name of the link to the related resource.")
    href: str = Field(..., description="The URI to the related resource.")


class OracleORCEducation(BaseModel):
    Description: t.Optional[str] = Field(
        None, description="Description in recruiting candidate education."
    )
    Reimbursements: t.Optional[str] = Field(
        None, description="Reimbursements in recruiting candidate education."
    )
    ActivitySummary: t.Optional[str] = Field(
        None, description="Activity summary in recruiting candidate education."
    )
    Educator: t.Optional[str] = Field(
        None, description="Educator in recruiting candidate education.", max_length=240
    )
    CompletedTraningUnits: t.Optional[str] = Field(
        None,
        description="Completed training units in recruiting candidate education.",
        max_length=240,
    )
    StartDate: t.Optional[str] = Field(
        None, description="Start date in recruiting candidate education."
    )
    ContentItemId: t.Optional[int] = Field(
        None, description="Content item ID in recruiting candidate education."
    )
    Department: t.Optional[str] = Field(
        None,
        description="Department in recruiting candidate education.",
        max_length=240,
    )
    CompletedAmount: t.Optional[int] = Field(
        None, description="Completed amount in recruiting candidate education."
    )
    GraduatedFlag: t.Optional[bool] = Field(
        None, description="Graduated indicator in recruiting candidate education."
    )
    TrmnlDegDiscpFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Terminal degree discipline indicator indicating the last degree obtained"
            " in recruiting candidate education."
        ),
    )
    EducationId: int = Field(
        -1, description="Education ID in recruiting candidate education."
    )
    AwardingBody: t.Optional[str] = Field(
        None,
        description="Awarding body in recruiting candidate education.",
        max_length=240,
    )
    CountryCode: t.Optional[str] = Field(
        None,
        description="Country code number in recruiting candidate education.",
        max_length=30,
    )
    CountryId: t.Optional[int] = Field(
        None, description="Country ID in recruiting candidate education."
    )
    Status: t.Optional[str] = Field(
        None, description="Status in recruiting candidate education.", max_length=30
    )
    HighestEduLevel: t.Optional[str] = Field(
        None,
        description="Highest education level in recruiting candidate education.",
        max_length=30,
    )
    StateProvinceId: t.Optional[int] = Field(
        None, description="State province ID in recruiting candidate education."
    )
    Comments: t.Optional[str] = Field(
        None, description="Comments in recruiting candidate education."
    )
    EducationURL: t.Optional[str] = Field(
        None,
        description="Education URL in recruiting candidate education.",
        max_length=2000,
    )
    Fee: t.Optional[int] = Field(
        None, description="Fee in recruiting candidate education."
    )
    PartTimeFlag: t.Optional[bool] = Field(
        None, description="Part time indicator in recruiting candidate education."
    )
    StateProvinceCode: t.Optional[str] = Field(
        None,
        description="State province code in recruiting candidate education.",
        max_length=360,
    )
    Title: t.Optional[str] = Field(
        None, description="Title in recruiting candidate education.", max_length=240
    )
    Duration: t.Optional[str] = Field(
        None, description="Duration in recruiting candidate education.", max_length=240
    )
    DurationUnits: t.Optional[str] = Field(
        None,
        description="Duration units in recruiting candidate education.",
        max_length=30,
    )
    City: t.Optional[str] = Field(
        None, description="City in recruiting candidate education.", max_length=240
    )
    TotalAmount: t.Optional[int] = Field(
        None, description="Total amount in recruiting candidate education."
    )
    AcademicStanding: t.Optional[str] = Field(
        None,
        description="Academic standing in recruiting candidate education.",
        max_length=30,
    )
    EducationLevel: t.Optional[str] = Field(
        None,
        description="Education level in recruiting candidate education.",
        max_length=30,
    )
    EndDate: t.Optional[str] = Field(
        None, description="End date in recruiting candidate education."
    )
    TuitionMethod: t.Optional[str] = Field(
        None,
        description="Tuition method in recruiting candidate education.",
        max_length=30,
    )
    EduLevelCompletedFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Education level completed indicator in recruiting candidate education."
        ),
    )
    Major: t.Optional[str] = Field(
        None, description="Major in recruiting candidate education.", max_length=240
    )
    DateAcquired: t.Optional[str] = Field(
        None, description="Date acquired in recruiting candidate education."
    )
    ProjectedCompletionDate: t.Optional[str] = Field(
        None, description="Projected completion in recruiting candidate education."
    )
    EducationalEstablishment: t.Optional[str] = Field(
        None,
        description="Education establishment in recruiting candidate education.",
        max_length=2000,
    )
    EducationalEstablishmentId: t.Optional[int] = Field(
        None,
        description="Education establishment ID in recruiting candidate education.",
    )
    AverageGrade: t.Optional[str] = Field(
        None,
        description="Average grade in recruiting candidate education.",
        max_length=240,
    )
    DegreeName: t.Optional[str] = Field(
        None,
        description="Degree name in recruiting candidate education.",
        max_length=240,
    )
    GPA: t.Optional[float] = Field(
        None, description="GPA in recruiting candidate education."
    )
    RequiredFlag: t.Optional[bool] = Field(
        None, description="Required indicator in recruiting candidate education."
    )
    Minor: t.Optional[str] = Field(
        None, description="Minor in recruiting candidate education.", max_length=240
    )
    SectionId: t.Optional[int] = Field(
        None, description="Section ID in recruiting candidate education."
    )
    YearAcquired: t.Optional[int] = Field(
        None, description="Year acquired in recruiting candidate education."
    )
    AreaOfStudy: t.Optional[str] = Field(
        None,
        description="Area of study in recruiting candidate education.",
        max_length=240,
    )
    FeeCurrency: t.Optional[str] = Field(
        None,
        description="Fee currency in recruiting candidate education.",
        max_length=30,
    )

    class Config:
        validate_assignment = True


class OracleORCCandidateAddress(BaseModel):
    Building: t.Optional[str] = Field(
        None, description="Building in recruiting candidate address.", max_length=240
    )
    FloorNumber: t.Optional[str] = Field(
        None, description="Floor number in recruiting candidate address.", max_length=40
    )
    LongPostalCode: t.Optional[str] = Field(
        None,
        description="Long postal code in recruiting candidate address.",
        max_length=30,
    )
    Region1: t.Optional[str] = Field(
        None, description="Region 1 in recruiting candidate address.", max_length=120
    )
    AddlAddressAttribute2: t.Optional[str] = Field(
        None,
        description="Add Address Attribute 2 in recruiting candidate address.",
        max_length=150,
    )
    PostalCode: t.Optional[str] = Field(
        None, description="Postal code in recruiting candidate address.", max_length=30
    )
    AddlAddressAttribute1: t.Optional[str] = Field(
        None, description="Link Relations", max_length=150
    )
    Region3: t.Optional[str] = Field(
        None, description="Region 3 in recruiting candidate address.", max_length=120
    )
    City: t.Optional[str] = Field(
        None, description="City in recruiting candidate address.", max_length=60
    )
    Region2: t.Optional[str] = Field(
        None, description="Region 2 in recruiting candidate address.", max_length=120
    )
    AddressLine3: t.Optional[str] = Field(
        None,
        description="Address Line 3 in recruiting candidate address.",
        max_length=240,
    )
    AddressLine2: t.Optional[str] = Field(
        None,
        description="Address Line 2 in recruiting candidate address.",
        max_length=240,
    )
    AddressLine1: t.Optional[str] = Field(
        None,
        description="Address Line 1 in recruiting candidate address.",
        max_length=240,
    )
    AddlAddressAttribute5: t.Optional[str] = Field(
        None,
        description="Add Address Attribute 5 in recruiting candidate address.",
        max_length=150,
    )
    AddlAddressAttribute4: t.Optional[str] = Field(
        None,
        description="Add Address Attribute 4 in recruiting candidate address.",
        max_length=150,
    )
    AddlAddressAttribute3: t.Optional[str] = Field(
        None,
        description="Add Address Attribute 3 in recruiting candidate address.",
        max_length=150,
    )
    Country: t.Optional[str] = Field(
        None, description="Country in recruiting candidate address.", max_length=60
    )

    class Config:
        validate_assignment = True


class OracleORCExperience(BaseModel):
    StartingPosition: t.Optional[str] = Field(
        None,
        description="Starting position in recruiting candidate experience.",
        max_length=240,
    )
    OtherJobFunction: t.Optional[str] = Field(
        None,
        description="Other job function in recruiting candidate experience.",
        max_length=240,
    )
    JobFunction: t.Optional[str] = Field(
        None,
        description="Job function in recruiting candidate experience.",
        max_length=30,
    )
    OtherCompensation: t.Optional[str] = Field(
        None,
        description="Other compensation in recruiting candidate experience.",
        max_length=240,
    )
    DirectReports: t.Optional[int] = Field(
        None, description="Direct reports in recruiting candidate experience."
    )
    Responsibilities: t.Optional[str] = Field(
        None, description="Responsibilities in recruiting candidate experience."
    )
    BusinessType: t.Optional[str] = Field(
        None,
        description="Business type in recruiting candidate experience.",
        max_length=240,
    )
    JobTitle: t.Optional[str] = Field(
        None,
        description="Job title in recruiting candidate experience.",
        max_length=240,
    )
    EmployerCountryId: t.Optional[int] = Field(
        None, description="Employer country ID in recruiting candidate experience."
    )
    SupervisorPhone: t.Optional[str] = Field(
        None,
        description="Supervisor phone in recruiting candidate experience.",
        max_length=240,
    )
    StartDate: t.Optional[str] = Field(
        None, description="Start date in recruiting candidate experience."
    )
    EmployerCity: t.Optional[str] = Field(
        None,
        description="Employer city in recruiting candidate experience.",
        max_length=240,
    )
    EmployerStateId: t.Optional[int] = Field(
        None, description="Employer state ID in recruiting candidate experience."
    )
    Department: t.Optional[str] = Field(
        None,
        description="Department in recruiting candidate experience.",
        max_length=240,
    )
    ContactSupervisorFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Contact supervisor indicator in the recruiting candidate experience."
        ),
    )
    CompanyURL: t.Optional[str] = Field(
        None,
        description="Company URL in recruiting candidate experience.",
        max_length=2000,
    )
    SupervisorEmail: t.Optional[str] = Field(
        None,
        description="Supervisor email in recruiting candidate experience.",
        max_length=240,
    )
    CountryCode: t.Optional[str] = Field(
        None,
        description="Country code number in recruiting candidate experience.",
        max_length=30,
    )
    FullTimeCode: t.Optional[str] = Field(
        None,
        description="Full time code in recruiting candidate experience.",
        max_length=30,
    )
    EmployerPhone: t.Optional[str] = Field(
        None,
        description="Employer phone in recruiting candidate experience.",
        max_length=240,
    )
    AdditionalInformation: t.Optional[str] = Field(
        None, description="Additional Information in recruiting candidate experience."
    )
    StartCompensation: t.Optional[float] = Field(
        None, description="Start compensation in recruiting candidate experience."
    )
    LeavingReason: t.Optional[str] = Field(
        None, description="Leaving reason in recruiting candidate experience."
    )
    PreviousEmploymentId: int = Field(
        -1, description="Previous employment ID in recruiting candidate experience."
    )
    StateProvinceCode: t.Optional[str] = Field(
        None,
        description="State province code in recruiting candidate experience.",
        max_length=360,
    )
    InternalFlag: t.Optional[bool] = Field(
        None, description="Internal indicator in the recruiting candidate experience."
    )
    SupervisorName: t.Optional[str] = Field(
        None,
        description="Supervisor name in recruiting candidate experience.",
        max_length=240,
    )
    EndDate: t.Optional[str] = Field(
        None, description="End date in recruiting candidate experience."
    )
    SupervisorTitle: t.Optional[str] = Field(
        None,
        description="Supervisor title in recruiting candidate experience.",
        max_length=240,
    )
    JobFamilyId: t.Optional[int] = Field(
        None, description="Job family ID in recruiting candidate experience."
    )
    Achievements: t.Optional[str] = Field(
        None, description="Achievements in recruiting candidate experience."
    )
    CurrentJobFlag: t.Optional[bool] = Field(
        None, description="Current job indicator in recruiting candidate experience."
    )
    EmployerName: t.Optional[str] = Field(
        None,
        description="Employer name in recruiting candidate experience.",
        max_length=240,
    )
    SectionId: t.Optional[int] = Field(
        None, description="Section ID in recruiting candidate experience."
    )
    EndCompensation: t.Optional[float] = Field(
        None, description="End compensation in recruiting candidate experience."
    )

    class Config:
        validate_assignment = True


class OracleORCSkill(BaseModel):
    YearsOfExperience: t.Optional[int] = Field(
        None, description="Years of experience in recruiting candidate skills."
    )
    Skill: str = Field(
        ...,
        description="Skills described in recruiting candidate skills.",
        max_length=240,
    )
    DateAchieved: t.Optional[str] = Field(
        None,
        description=(
            "Date when the skills were achieved in recruiting candidate skills."
        ),
    )
    Speciality: t.Optional[str] = Field(
        None,
        description="Specialty or skill type in recruiting candidate skills.",
        max_length=30,
    )
    Description: t.Optional[str] = Field(
        None, description="Description of recruiting candidate skills."
    )
    ProjectName: t.Optional[str] = Field(
        None,
        description="Project name or activities in recruiting candidate skills.",
        max_length=2000,
    )
    Comments: t.Optional[str] = Field(
        None, description="Comments in recruiting candidate skills."
    )
    SkillId: int = Field(
        -1,
        description=(
            "ID assigned to each skill described in recruiting candidate skills."
        ),
    )
    SectionId: int = Field(-1, description="Section ID in recruiting candidate skills.")

    class Config:
        validate_assignment = True


class OracleORCLicenseAndCertificate(BaseModel):
    RenewalRequiredFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Renewal required indicator in recruiting candidate licenses and"
            " certificates."
        ),
    )
    OriginalIssueYear: t.Optional[int] = Field(
        None,
        description=(
            "Original issue year in recruiting candidate licenses and certificates."
        ),
    )
    ContentItemId: t.Optional[int] = Field(
        None,
        description=(
            "Content item ID in recruiting candidate licenses and certificates."
        ),
    )
    CompletedAmount: t.Optional[int] = Field(
        None,
        description=(
            "Completed amount in recruiting candidate licenses and certificates."
        ),
    )
    CountryCode: t.Optional[str] = Field(
        None,
        description=(
            "Country code number in recruiting candidate licenses and certificates."
        ),
        max_length=30,
    )
    CompletedTrainingUnits: t.Optional[str] = Field(
        None,
        description=(
            "Completed training units in recruiting candidate licenses and"
            " certificates."
        ),
        max_length=240,
    )
    ActualCompletionDate: t.Optional[str] = Field(
        None,
        description=(
            "Actual completion date in recruiting candidate licenses and certificates."
        ),
    )
    Restrictions: t.Optional[str] = Field(
        None,
        description="Restrictions in recruiting candidate licenses and certificates.",
    )
    CountryId: t.Optional[int] = Field(
        None,
        description="Country ID in recruiting candidate licenses and certificates.",
    )
    Status: t.Optional[str] = Field(
        None,
        description="Status in recruiting candidate licenses and certificates.",
        max_length=30,
    )
    CertificationURL: t.Optional[str] = Field(
        None,
        description=(
            "Certification URL in recruiting candidate licenses and certificates."
        ),
        max_length=2000,
    )
    VerifiedFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Verified indicator in recruiting candidate licenses and certificates."
        ),
    )
    RenewalInprogressFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Renewal in progress indicator in recruiting candidate licenses and"
            " certificates."
        ),
    )
    StateProvinceId: t.Optional[int] = Field(
        None,
        description=(
            "State province ID in recruiting candidate licenses and certificates."
        ),
    )
    Comments: t.Optional[str] = Field(
        None, description="Comments in recruiting candidate licenses and certificates."
    )
    StateProvinceCode: t.Optional[str] = Field(
        None,
        description=(
            "State province code in recruiting candidate licenses and certificates."
        ),
        max_length=360,
    )
    IssueDate: t.Optional[str] = Field(
        None,
        description="Issue date in recruiting candidate licenses and certificates.",
    )
    Title: t.Optional[str] = Field(
        None,
        description="Title in recruiting candidate licenses and certificates.",
        max_length=240,
    )
    TotalAmount: t.Optional[int] = Field(
        None,
        description="Total amount in recruiting candidate licenses and certificates.",
    )
    LastRenewalDate: t.Optional[str] = Field(
        None,
        description=(
            "Last renewal date in recruiting candidate licenses and certificates."
        ),
    )
    CertificationId: int = Field(
        ...,
        description=(
            "Certification ID in recruiting candidate licenses and certificates."
        ),
    )
    RenewalDate: t.Optional[str] = Field(
        None,
        description="Renewal date in recruiting candidate licenses and certificates.",
    )
    CertificationName: t.Optional[str] = Field(
        None,
        description=(
            "Certification name in recruiting candidate licenses and certificates."
        ),
        max_length=240,
    )
    ExpirationDate: t.Optional[str] = Field(
        None,
        description=(
            "Expiration date in recruiting candidate licenses and certificates."
        ),
    )
    EducationalEstablishment: t.Optional[str] = Field(
        None,
        description=(
            "Educational establishment in recruiting candidate licenses and"
            " certificates."
        ),
        max_length=2000,
    )
    EducationalEstablishmentId: t.Optional[int] = Field(
        None,
        description=(
            "Educational establishment ID in recruiting candidate licenses and"
            " certificates."
        ),
    )
    CertificationNumber: t.Optional[str] = Field(
        None,
        description=(
            "Certification number in recruiting candidate licenses and certificates."
        ),
        max_length=240,
    )
    IssuedBy: t.Optional[str] = Field(
        None,
        description="Issued using in recruiting candidate licenses and certificates.",
        max_length=240,
    )
    SectionId: t.Optional[int] = Field(
        None,
        description="Section ID in recruiting candidate licenses and certificates.",
    )

    class Config:
        validate_assignment = True


class OracleORCLanguage(BaseModel):
    AbleToTranslateFlag: t.Optional[bool] = Field(
        None,
        description="Able to translate indicator in recruiting candidate languages.",
    )
    AbleToTeachFlag: t.Optional[bool] = Field(
        None, description="Able to teach indicator in recruiting candidate languages."
    )
    ReadingModelId: t.Optional[int] = Field(
        None, description="Reading model ID in recruiting candidate languages."
    )
    Comments: t.Optional[str] = Field(
        None, description="Comments in recruiting candidate languages."
    )
    SpeakingModelId: t.Optional[int] = Field(
        None, description="Speaking model ID in recruiting candidate languages."
    )
    SpeakingLevelId: t.Optional[int] = Field(
        None, description="Speaking level ID in recruiting candidate languages."
    )
    NativeSpeakerFlag: t.Optional[bool] = Field(
        None, description="Native speaker indicator in recruiting candidate languages."
    )
    ReadingLevelId: t.Optional[int] = Field(
        None, description="Reading level ID in recruiting candidate languages."
    )
    ContentItemId: t.Optional[int] = Field(
        None, description="Content item ID in recruiting candidate languages."
    )
    WritingLevelId: t.Optional[int] = Field(
        None, description="Writing level ID in recruiting candidate languages."
    )
    Language: t.Optional[str] = Field(
        None, description="Language in recruiting candidate languages.", max_length=240
    )
    EvaluatedOn: t.Optional[str] = Field(
        None, description="Evaluated on in recruiting candidate languages."
    )
    WritingModelId: t.Optional[int] = Field(
        None, description="Writing model ID in recruiting candidate languages."
    )
    SectionId: t.Optional[int] = Field(
        None, description="Section ID in recruiting candidate languages."
    )
    EvaluationLocation: t.Optional[str] = Field(
        None,
        description="Evaluation location in recruiting candidate languages.",
        max_length=240,
    )
    LanguageId: int = Field(
        ..., description="Language ID in recruiting candidate languages."
    )

    class Config:
        validate_assignment = True


class OracleORCCandidatePhone(BaseModel):
    AreaCode: t.Optional[str] = Field(
        None, description="Area code in recruiting candidate phone.", max_length=30
    )
    PhoneNumber: t.Optional[str] = Field(
        None, description="Phone number in recruiting candidate phone.", max_length=60
    )
    LegislationCode: t.Optional[str] = Field(
        None,
        description="Legislation code in recruiting candidate phone.",
        max_length=4,
    )
    CountryCodeNumber: t.Optional[str] = Field(
        None,
        description="Country code number in recruiting candidate phone.",
        max_length=30,
    )

    class Config:
        validate_assignment = True


class OracleORCWorkPreference(BaseModel):
    NatTravelRequiredFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Domestic travel required indicator in recruiting candidate work"
            " preferences."
        ),
    )
    FlexibleWorkFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Flexible work required indicator in recruiting candidate work preferences."
        ),
    )
    NatTravelFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Domestic travel required indicator in the recruiting candidate work"
            " preferences."
        ),
    )
    RelocateFlag: t.Optional[bool] = Field(
        None,
        description="Relocate indicator in the recruiting candidate work preferences.",
    )
    WorkDays: t.Optional[str] = Field(
        None,
        description="Work days in recruiting candidate work preferences.",
        max_length=30,
    )
    ExcludedLocationTwoId: t.Optional[int] = Field(
        None,
        description=(
            "Excluded location two ID in recruiting candidate work preferences."
        ),
    )
    NatTravelFrequency: t.Optional[str] = Field(
        None,
        description=(
            "National travel frequency in recruiting candidate work preferences."
        ),
        max_length=30,
    )
    AllLocationsFlag: t.Optional[bool] = Field(
        None,
        description=(
            "All locations preferred indicator in recruiting candidate work"
            " preferences."
        ),
    )
    CurrencyCode: t.Optional[str] = Field(
        None,
        description="Currency code in recruiting candidate work preferences.",
        max_length=30,
    )
    ContentItemId: t.Optional[int] = Field(
        None, description="Content item ID in recruiting candidate work preferences."
    )
    PayFrequency: t.Optional[str] = Field(
        None,
        description="Pay frequency in recruiting candidate work preferences.",
        max_length=30,
    )
    PayRange: t.Optional[str] = Field(
        None,
        description="Pay range in recruiting candidate work preferences.",
        max_length=30,
    )
    WorkMonths: t.Optional[int] = Field(
        None, description="Work months in recruiting candidate work preferences."
    )
    ExcludedLocationOneId: t.Optional[int] = Field(
        None,
        description=(
            "Excluded location one ID in recruiting candidate work preferences."
        ),
    )
    TempAssignFlag: t.Optional[bool] = Field(
        None,
        description="Temp assign indicator in recruiting candidate work preferences.",
    )
    PreferredLocationTwoId: t.Optional[int] = Field(
        None,
        description=(
            "Preferred location two ID in recruiting candidate work preferences."
        ),
    )
    ExcludedLocationThreeId: t.Optional[int] = Field(
        None,
        description=(
            "Excluded location three ID in recruiting candidate work preferences."
        ),
    )
    PreferredLocationFourId: t.Optional[int] = Field(
        None,
        description=(
            "Preferred location four ID in recruiting candidate work preferences."
        ),
    )
    MinimumPay: t.Optional[int] = Field(
        None, description="Minimum pay in recruiting candidate work preferences."
    )
    PreferredLocationThreeId: t.Optional[int] = Field(
        None,
        description=(
            "Preferred location three ID in recruiting candidate work preferences."
        ),
    )
    Comments: t.Optional[str] = Field(
        None, description="Comments in recruiting candidate work preferences."
    )
    FullTimeEquivalent: t.Optional[float] = Field(
        None,
        description="Full time equivalent in recruiting candidate work preferences.",
    )
    IntlTravelFrequency: t.Optional[str] = Field(
        None,
        description=(
            "International travel frequency in recruiting candidate work preferences."
        ),
        max_length=30,
    )
    WorkPreferenceId: int = Field(
        ..., description="Work preference ID in recruiting candidate work preferences."
    )
    PreferredLocationOneId: t.Optional[int] = Field(
        None,
        description=(
            "Preferred location one ID in recruiting candidate work preferences."
        ),
    )
    ExcludedLocationFourId: t.Optional[int] = Field(
        None,
        description=(
            "Excluded location four ID in recruiting candidate work preferences."
        ),
    )
    PartAssignFlag: t.Optional[bool] = Field(
        None,
        description=(
            "Part assign indicator in the recruiting candidate work preferences."
        ),
    )
    RelocateDuration: t.Optional[int] = Field(
        None, description="Relocate duration in recruiting candidate work preferences."
    )
    WorkYears: t.Optional[int] = Field(
        None, description="Work years in recruiting candidate work preferences."
    )
    RelocationReason: t.Optional[str] = Field(
        None,
        description="Relocation reason in recruiting candidate work preferences.",
        max_length=240,
    )
    IntlTravelFlag: t.Optional[bool] = Field(
        None,
        description=(
            "International travel required indicator in recruiting candidate work"
            " preferences."
        ),
    )
    WorkHours: t.Optional[str] = Field(
        None,
        description="Work hours in recruiting candidate work preferences.",
        max_length=30,
    )
    DateFrom: str = Field(
        ..., description="Date from in recruiting candidate work preferences."
    )
    IntlTravelRequiredFlag: t.Optional[bool] = Field(
        None,
        description=(
            "International travel required indicator in the recruiting candidate work"
            " preferences."
        ),
    )
    SectionId: t.Optional[int] = Field(
        None, description="Section ID in recruiting candidate work preferences."
    )
    DateTo: t.Optional[str] = Field(
        None, description="Date to in recruiting candidate work preferences."
    )

    class Config:
        validate_assignment = True


class OracleORCCandidate(BaseModel):
    CreationDate: t.Optional[str] = Field(
        None, description="Creation date in recruiting candidates."
    )
    CandLastModifiedDate: t.Optional[str] = Field(
        None, description="Candidate Last Modified Date"
    )
    PreNameAdjunct: t.Optional[str] = Field(
        None,
        description="Previous name adjunct in recruiting candidates.",
        max_length=150,
    )
    Email: t.Optional[str] = Field(
        None, description="Email in recruiting candidates.", max_length=240
    )
    attachments: t.Optional[t.List[OracleORCAttachment]] = Field(
        None,
        description=(
            "The attachments resource is used to view, create, and update attachments."
        ),
    )
    education: t.Optional[t.List[OracleORCEducation]] = Field(
        None,
        description=(
            "The education resource is a child of the recruitingCandidates resource and"
            " provides details about the education of the candidate."
        ),
    )
    Honors: t.Optional[str] = Field(
        None, description="Honors in recruiting candidates.", max_length=80
    )
    candidateAddress: t.Optional[t.List[OracleORCCandidateAddress]] = Field(
        None,
        description=(
            "The candidateAddress resource is a child of the recruitingCandidates"
            " resource and provides details about the address of the candidate."
        ),
    )
    KnownAs: t.Optional[str] = Field(
        None, description="Known as in recruiting candidates.", max_length=80
    )
    SourceMedium: t.Optional[str] = Field(
        None, description="Source medium in recruiting candidates.", max_length=32
    )
    experience: t.Optional[t.List[OracleORCExperience]] = Field(
        None,
        description=(
            "The experience resource is a child of the recruitingCandidates resource"
            " and provides details about the work experience of the candidate."
        ),
    )
    skills: t.Optional[t.List[OracleORCSkill]] = Field(
        None,
        description=(
            "The skills resource is a child of the recruitingCandidates resource and"
            " provides details about the skills of the candidate."
        ),
    )
    licensesAndCertificates: t.Optional[t.List[OracleORCLicenseAndCertificate]] = Field(
        None,
        description=(
            "The licensesAndCertificates resource is a child of the"
            " recruitingCandidates resource and provides details about the licenses"
            " and certificates of the candidate."
        ),
    )
    PreferredTimezone: t.Optional[str] = Field(
        None, description="Preferred time zone of the candidate.", max_length=255
    )
    PreferredLanguage: t.Optional[str] = Field(
        None, description="Preferred language in recruiting candidates.", max_length=4
    )
    MiddleNames: t.Optional[str] = Field(
        None, description="Middle names in recruiting candidates.", max_length=80
    )
    MilitaryRank: t.Optional[str] = Field(
        None, description="Military rank in recruiting candidates.", max_length=80
    )
    LastUpdatedBy: t.Optional[str] = Field(
        None, description="Last update using in recruiting candidates."
    )
    workPreferences: t.Optional[t.List[OracleORCWorkPreference]] = Field(
        None,
        description=(
            "The workPreferences resource is a child of the recruitingCandidates"
            " resource and provides details about the work preferences of the"
            " candidate."
        ),
    )
    CampaignOptIn: t.Optional[str] = Field(
        None, description="Campaign opt-in feature to recruit candidates.", max_length=1
    )
    CreatedBy: t.Optional[str] = Field(
        None, description="Created using in recruiting candidates."
    )
    languages: t.Optional[t.List[OracleORCLanguage]] = Field(
        None,
        description=(
            "The languages resource is a child of the recruitingCandidates resource and"
            " provides details about the languages of the candidate."
        ),
    )
    FirstName: t.Optional[str] = Field(
        None, description="First name in recruiting candidates.", max_length=150
    )
    PreviousLastName: t.Optional[str] = Field(
        None, description="Previous last name in recruiting candidates.", max_length=150
    )
    candidatePhones: t.Optional[t.List[OracleORCCandidatePhone]] = Field(
        None,
        description=(
            "The candidatePhones resource is a child of the recruitingCandidates"
            " resource and provides details about the phone numbers of the candidate."
        ),
    )
    Title: t.Optional[str] = Field(
        None, description="Title in recruiting candidates.", max_length=30
    )
    SourceName: t.Optional[str] = Field(
        None, description="Source name in recruiting candidates.", max_length=2000
    )
    Suffix: t.Optional[str] = Field(
        None, description="Suffix in recruiting candidates.", max_length=80
    )
    LastName: t.Optional[str] = Field(
        None, description="Last name in recruiting candidates.", max_length=150
    )
    LastUpdateDate: t.Optional[str] = Field(
        None, description="Last update date in recruiting candidates."
    )

    class Config:
        validate_assignment = True
