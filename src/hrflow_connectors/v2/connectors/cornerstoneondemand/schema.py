import typing as t

from msgspec import Struct


class AddressDetailsStruct(Struct):
    City: str
    Country: str
    Line1: str
    Line2: str
    PostalCode: str
    State: str


class SiteStruct(Struct):
    EffectiveDate: str
    ExpirationDate: str
    Name: str
    URL: str


class UserStruct(Struct):
    Id: int
    Name: str
    Ref: str


class LocationStruct(Struct):
    Address: str
    AddressDetails: AddressDetailsStruct


class CornerstoneJobRequisition(Struct):
    AdditionalLocations: t.List[LocationStruct]
    Address: str
    AddressDetails: AddressDetailsStruct
    ApplicantCount: int
    CanAppy: bool
    CareerSites: t.List[SiteStruct]
    Compensation: str
    ContactPhone: str
    CreateDateLocal: str
    Currency: str
    CurrencySymbol: str
    DaysOpen: int
    DefaultEffectiveDate: str
    DefaultExpirationDate: str
    DefaultLanguage: str
    DefaultName: str
    DefaultURL: str
    Division: str
    DivisionId: str
    EEOCategory: str
    EmploymentStatus: str
    EmploymentType: str
    ExternalAd: str
    ExternalDescription: str
    Grade: str
    GradeId: str
    HiringManager: UserStruct
    Id: int
    IdealQualification: str
    InternalAd: str
    InternalDescription: str
    JobResponsibilities: t.List[str]
    Keywords: str
    LastModificationDate: str
    Location: str
    LocationId: str
    MetaPageDesc: str
    MetaPageTitle: str
    MinimumQualification: str
    MobileAd: str
    NewSubmissionCount: int
    Ongoing: bool
    OpenDateLocal: str
    OpenPostingCount: int
    Openings: int
    Position: str
    PositionId: str
    Priority: str
    PriorityName: str
    RangeHigh: int
    RangeLow: int
    Ref: str
    ReferalBonus: int
    RequisitionTemplate: str
    RequisitionTemplateID: int
    Status: str
    SuggestedReferralCount: int
    TargetHireDate: str
    Title: str


class CornerstoneApplicant(Struct):
    AddressLine1: str
    AddressLine2: str
    ApplicantHireDate: str
    ApplicationReceivedDateLocal: str
    AverageRating: int
    CandidateType: str
    City: str
    ConsiderForOtherJobs: bool
    CostCenterId: str
    CostCenterTitle: str
    Country: str
    CsodGUID: str
    DivisionId: str
    DivisionTitle: str
    Email: str
    Ethnicity: str
    Fax: str
    FirstName: str
    Gender: str
    GradeId: str
    GradeTitle: str
    HomePhone: str
    Id: int
    JobRequisitionId: int
    LastName: str
    Name: str
    Phone: str
    PositionId: str
    PositionTitle: str
    PostalCode: str
    PreviousStatus: str
    RequisitionId: str
    RequisitionName: str
    RequisitionOwners: UserStruct
    Source: str
    State: str
    Status: str
    ThumbImgUrl: str
    UserId: str
    Username: str
