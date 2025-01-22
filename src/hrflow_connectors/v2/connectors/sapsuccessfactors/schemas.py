from typing import Any, Dict, List, Optional

from msgspec import Struct


class SAPSuccessFactorsJobReqLocale(Struct):
    jobDescription: Optional[str]
    jobTitle: Optional[str]
    jobReqId: Optional[str]
    status: Optional[str]


# Job model
class SAPSuccessFactorsJobRequistion(Struct):
    accommodation_cost: Optional[str]
    age: str
    appTemplateId: str
    approver: List[Dict[str, Any]]
    assessRatingScaleName: str
    businessUnit_obj: Optional[Dict[str, Any]]
    candidateProgress: str
    classificationTime: Optional[str]
    classificationType: Optional[str]
    closedDateTime: Optional[str]
    comment: Optional[str]
    commentEquip: Optional[str]
    commission: Optional[str]
    corporatePosting: Optional[bool]
    costCenterId: Optional[str]
    costOfHire: Optional[str]
    country: Optional[str]
    createdDateTime: Optional[str]
    currency: Optional[str]
    customString10: Optional[str]
    customString3: Optional[str]
    customString4: Optional[str]
    customString8: Optional[str]
    defaultLanguage: Optional[str]
    deleted: str
    department_obj: Optional[str]
    division_obj: Optional[str]
    erpAmount: Optional[str]
    evergreen: Optional[bool]
    experienceReq: Optional[str]
    facility: Optional[str]
    formDataId: str
    formDueDate: Optional[str]
    hiringManagerNote: Optional[str]
    industry: Optional[str]
    instrCompQuest: Optional[str]
    instrContractDetails: Optional[str]
    instrCostHire: Optional[str]
    instrDescription: Optional[str]
    instrERS: Optional[str]
    instrEquest: Optional[str]
    instrEquipment: Optional[str]
    instrGen: Optional[str]
    instrInterview: Optional[str]
    instrInvolvedParties: Optional[str]
    instrInvolvedParties1: Optional[str]
    instrJobDetails: Optional[str]
    instrJobReq: Optional[str]
    instrManagernote: Optional[str]
    instrOverallComments: Optional[str]
    instrPD: Optional[str]
    instrReq: Optional[str]
    instrReqDates: Optional[str]
    instrReqDetails: Optional[str]
    instrReqJustify: Optional[str]
    instrReqRequire: Optional[str]
    instrSalary: Optional[str]
    instr_rec_cost: Optional[str]
    internalStatus: str
    interviewGuide: Optional[str]
    intranetPosting: Optional[bool]
    isDraft: Optional[bool]
    jobCode: Optional[str]
    jobProfile: Optional[str]
    jobReqGUId: str
    jobReqId: str
    jobRole: Optional[str]
    jobStartDate: Optional[str]
    lastModifiedBy: str
    lastModifiedDateTime: Optional[str]
    lastModifiedProxyUserId: str
    legalEntity_obj: Optional[str]
    locationCode: Optional[str]
    location_obj: Optional[str]
    misc_cost: Optional[str]
    motorVeh: Optional[str]
    numberOpenings: Optional[str]
    openingsFilled: Optional[str]
    otherBonus: Optional[str]
    otherCompensation: Optional[str]
    otherEquip: Optional[str]
    othrComms: Optional[str]
    overallScaleName: str
    positionNumber: str
    postalcode: Optional[str]
    ratedApplicantCount: str
    recruitJust: Optional[str]
    replforwhom: Optional[str]
    restorehiringManagerTeamAdminDefaults: Optional[bool]
    restorerecruiterTeamAdminDefaults: Optional[bool]
    restoresourcerTeamAdminDefaults: Optional[bool]
    restorevpOfStaffingTeamAdminDefaults: Optional[bool]
    reverseScale: str
    salRateType: Optional[str]
    salaryBase: Optional[str]
    salaryComment: Optional[str]
    salaryMax: Optional[str]
    salaryMin: Optional[str]
    statusSetId: str
    stockPackage: Optional[str]
    targetBonusAmount: Optional[str]
    tempDate: Optional[str]
    templateId: str
    templateType: str
    timeToFill: str
    total_earnings: Optional[str]
    total_fixed_pay: Optional[str]
    total_hire_cost: Optional[str]
    travel_cost: Optional[str]
    workHours: Optional[str]
    approver: List[Dict[str, Any]]
    coordinator: List[Dict[str, Any]]
    currentOwner: List[Dict[str, Any]]
    hiringManager: List[Dict[str, Any]]
    hiringManagerTeam: List[Dict[str, Any]]
    hiringManagerTeamGroup: List[Dict[str, Any]]
    jobReqLocale: SAPSuccessFactorsJobReqLocale
    jobReqPostings: List[Dict[str, Any]]
    jobReqScreeningQuestions: List[Dict[str, Any]]
    originator: List[Dict[str, Any]]
    questions: List[Dict[str, Any]]
    recruiter: List[Dict[str, Any]]
    recruiterTeam: List[Dict[str, Any]]
    recruiterTeamGroup: List[Dict[str, Any]]
    secondRecruiter: List[Dict[str, Any]]
    sourcer: List[Dict[str, Any]]
    sourcerTeam: List[Dict[str, Any]]
    sourcerTeamGroup: List[Dict[str, Any]]
    vpOfStaffing: List[Dict[str, Any]]
    vpOfStaffingTeam: List[Dict[str, Any]]
    vpOfStaffingTeamGroup: List[Dict[str, Any]]


# Profile model


class Result(Struct):
    endDate: Optional[str]
    school: str
    schoolAddress: str
    startDate: Optional[str]


class Education(Struct):
    results: List[Result]


class ResultLanguage(Struct):
    language: str
    readingProf: str
    speakingProf: str
    writingProf: str


class ResultOutsideWorkExperience(Struct):
    employer: Optional[str]
    employerAddress: str
    endDate: Optional[str]
    startDate: Optional[str]


class OutsideWorkExperience(Struct):
    results: List[ResultOutsideWorkExperience]


class InsideWorkExperienceResult(Struct):
    backgroundElementId: Optional[str]
    bgOrderPos: Optional[str]
    candidateId: Optional[str]
    department: Optional[str]
    endDate: Optional[str]
    lastModifiedDateTime: Optional[str]
    startDate: Optional[str]
    title: Optional[str]
    candidate: Optional[str]


class TalentPoolResults(Struct):
    startDate: Optional[str]
    talentPoolComments: Optional[str]
    talentPoolStatus: Optional[str]
    talentPoolitem: Optional[str]


class TalentPool(Struct):
    results: List[TalentPoolResults]


class SapCandidateModel(Struct):
    address: Optional[str]
    agreeToPrivacyStatement: Optional[str]
    anonymized: str
    anonymizedDateTime: Optional[str]
    candidateId: str
    candidateLocale: str
    cellPhone: Optional[str]
    city: Optional[str]
    consentToMarketing: Optional[str]
    contactEmail: Optional[str]
    country: Optional[str]
    coverLetter: Optional[str]
    creationDateTime: Optional[str]
    currentTitle: Optional[str]
    dataPrivacyId: str
    dateofAvailability: Optional[str]
    externalCandidate: bool
    firstName: Optional[str]
    lastLoginDateTime: Optional[str]
    lastModifiedDateTime: Optional[str]
    lastName: Optional[str]
    partnerMemberId: str
    partnerSource: str
    password: Optional[str]
    primaryEmail: str
    privacyAcceptDateTime: Optional[str]
    publicIntranet: bool
    shareProfile: str
    usersSysId: str
    visibilityOption: bool
    zip: Optional[str]
    state: Optional[str]
    candidateProfileExtension: Optional[Dict[str, Any]]
    certificates: Optional[Dict[str, Any]]
    education: Optional[Dict[str, Any]]
    insideWorkExperience: Optional[Dict[str, Any]]
    languages: Optional[Dict[str, Any]]
    outsideWorkExperience: Optional[Dict[str, Any]]
    tags: Optional[Dict[str, Any]]
    talentPool: Optional[Dict[str, Any]]
    talentPoolcorp: Optional[Dict[str, Any]]
