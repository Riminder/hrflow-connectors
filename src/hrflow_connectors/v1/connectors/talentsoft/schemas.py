import typing as t

from pydantic import BaseModel


class PersonalInformation(BaseModel):
    civility: t.Optional[str]
    middleName: t.Optional[str]
    title: t.Optional[str]
    address: t.Optional[str]
    city: t.Optional[str]
    postalCode: t.Optional[str]
    birthDate: t.Optional[str]
    country: t.Optional[str]
    skypeAccount: t.Optional[str]
    receiveSMS: t.Optional[str]
    phoneNumber2: t.Optional[str]
    professionalEmail: t.Optional[str]
    sex: t.Optional[str]
    nationalities: t.List[str]
    frenchDisabledWorkerStatus: t.Optional[str]
    frenchPriorityNeighbourhood: t.Optional[str]
    firstName: str
    lastName: str
    email: str
    phoneNumber: str


class Diplomas(BaseModel):
    educationLevel: str
    diplomaCode: t.Optional[str]
    specialisation: t.Optional[str]
    yearObtained: str
    college: str
    collegeCity: t.Optional[str]


class Language(BaseModel):
    language: str
    languageLevel: str


class Education(BaseModel):
    diplomas: t.List[Diplomas]
    studiedLanguages: t.List[Language]


class Experience(BaseModel):
    experienceLevel: t.Optional[str]
    Profile: t.Optional[str]
    contract: t.Optional[str]
    company: str
    function: str
    length: t.Optional[str]


class Experiences(BaseModel):
    experienceLevel: t.Optional[str]
    experienceList: t.List[Experience]


class Mobility(BaseModel):
    geographicalAreas: t.List[str]
    countries: t.List[str]
    regions: t.List[str]
    departments: t.List[str]


class Availability(BaseModel):
    acceptsExtra: t.Optional[str]
    values: t.List[str]


class FurtherInformation(BaseModel):
    skills: t.List[str]


class eEOInformation(BaseModel):
    doesNotComplete: bool
    sex: t.Optional[str]
    race: t.Optional[str]
    ethnicity: t.Optional[str]
    veteranStatus: t.Optional[str]
    incapacityStatus: t.Optional[str]


class jobPreferences(BaseModel):
    primaryProfile: t.Optional[str]
    contract: t.Optional[str]
    contractDuration: t.Optional[str]
    salaryPretensions: t.Optional[str]
    dateOfAvailability: t.Optional[str]
    mobility: Mobility
    noticeDuration: t.Optional[str]
    mobilityDelay: t.Optional[str]
    trainingDateStart: t.Optional[str]
    trainingDateEnd: t.Optional[str]
    jobTime: t.Optional[str]
    secondaryProfiles: t.List[str]
    availability: Availability


class standardItem(BaseModel):
    code: int
    clientCode: str
    label: str
    active: bool
    parentCode: t.Union[None, int]
    type: str
    parentType: str
    hasChildren: bool


class fileItem(BaseModel):
    guid: str
    name: str
    description: str
    fileType: standardItem


class Application(BaseModel):
    id: int
    type: str
    offerReference: str
    offerTitle: str
    isOfferPublished: bool
    organisation: standardItem
    origin: standardItem
    motivation: t.Optional[str]
    referralCode: t.Optional[str]
    files: t.List[fileItem]
    applicationAnswers: t.List[dict]
    date: str
    status: standardItem
    personalDataConsentReceived: t.Optional[str]
    retentionDelay: t.Optional[str]
    frenchDisabledWorkerStatus: t.Optional[str]


class UploadedFile(BaseModel):
    description: str
    fileTypeId: str
    key: str


class Applicant(BaseModel):
    personalInformation: PersonalInformation
    jobPreferences: jobPreferences
    educations: Education
    experiences: Experiences
    consents: t.List[dict]
    furtherInformation: FurtherInformation
    eEOInformation: eEOInformation
    customFields: str


class TalentsoftApplicantSchema(BaseModel):
    applicant: Applicant
    application: Application
    uploadedFiles: t.List[UploadedFile]


class UpdateSpecialEmploymentRegulationsInFrance(BaseModel):
    disabledWorkerStatus: bool
    priorityNeighbourhood: bool


class UpdatePersonalInformation(BaseModel):
    birthDate: str
    nationalities: t.List[str]
    address: str
    postalCode: str
    city: str
    residentCountryId: str
    specialEmploymentRegulationsInFrance: t.Optional[
        UpdateSpecialEmploymentRegulationsInFrance
    ]


class UpdateJobPreferences(BaseModel):
    primaryProfileId: str
    dateOfAvailability: str
    salaryExpectations: str


class UpdateLanguage(BaseModel):
    languageId: str
    languageLevelId: str


class UpdateEducation(BaseModel):
    diplomaId: str
    educationLevelId: str


class UpdateExperience(BaseModel):
    experienceLevelId: str
    profileId: str
    company: str
    function: str
    contractTypeId: str


class UpdateAttachment(BaseModel):
    description: str
    key: str
    fileType: str


class CandidateUpdated(BaseModel):
    employeeNumber: t.Optional[str]
    lastName: str
    firstName: str
    middleName: t.Optional[str]
    email: str
    phoneNumber: t.Optional[str]
    civilityId: t.Optional[str]
    personalInformation: t.Optional[UpdatePersonalInformation]
    jobPreferences: t.Optional[UpdateJobPreferences]
    languages: t.Optional[t.List[UpdateLanguage]]
    educations: t.Optional[t.List[UpdateEducation]]
    experiences: t.Optional[t.List[UpdateExperience]]
    attachments: t.Optional[t.List[UpdateAttachment]]
