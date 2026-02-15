/**
 * Talentsoft Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface PersonalInformation {
  civility: str | undefined;
  middleName: str | undefined;
  title: str | undefined;
  address: str | undefined;
  city: str | undefined;
  postalCode: str | undefined;
  birthDate: str | undefined;
  country: str | undefined;
  skypeAccount: str | undefined;
  receiveSMS: str | undefined;
  phoneNumber2: str | undefined;
  professionalEmail: str | undefined;
  sex: str | undefined;
  nationalities: str[;
  frenchDisabledWorkerStatus: str | undefined;
  frenchPriorityNeighbourhood: str | undefined;
  firstName: string;
  lastName: string;
  email: string;
  phoneNumber: string;
}

export interface Diplomas {
  educationLevel: string;
  diplomaCode: str | undefined;
  specialisation: str | undefined;
  yearObtained: string;
  college: string;
  collegeCity: str | undefined;
}

export interface Language {
  language: string;
  languageLevel: string;
}

export interface Education {
  diplomas: Diplomas[;
  studiedLanguages: Language[;
}

export interface Experience {
  experienceLevel: str | undefined;
  Profile: str | undefined;
  contract: str | undefined;
  company: string;
  function: string;
  length: str | undefined;
}

export interface Experiences {
  experienceLevel: str | undefined;
  experienceList: Experience[;
}

export interface Mobility {
  geographicalAreas: str[;
  countries: str[;
  regions: str[;
  departments: str[;
}

export interface Availability {
  acceptsExtra: str | undefined;
  values: str[;
}

export interface FurtherInformation {
  skills: str[;
}

export interface eEOInformation {
  doesNotComplete: boolean;
  sex: str | undefined;
  race: str | undefined;
  ethnicity: str | undefined;
  veteranStatus: str | undefined;
  incapacityStatus: str | undefined;
}

export interface jobPreferences {
  primaryProfile: str | undefined;
  contract: str | undefined;
  contractDuration: str | undefined;
  salaryPretensions: str | undefined;
  dateOfAvailability: str | undefined;
  mobility: Mobility;
  noticeDuration: str | undefined;
  mobilityDelay: str | undefined;
  trainingDateStart: str | undefined;
  trainingDateEnd: str | undefined;
  jobTime: str | undefined;
  secondaryProfiles: str[;
  availability: Availability;
}

export interface standardItem {
  code: number;
  clientCode: string;
  label: string;
  active: boolean;
  parentCode: t.Union[None, int;
  type: string;
  parentType: string;
  hasChildren: boolean;
}

export interface fileItem {
  guid: string;
  name: string;
  description: string;
  fileType: standardItem;
}

export interface Application {
  id: number;
  type: string;
  offerReference: string;
  offerTitle: string;
  isOfferPublished: boolean;
  organisation: standardItem;
  origin: standardItem;
  motivation: str | undefined;
  referralCode: str | undefined;
  files: fileItem[;
  applicationAnswers: dict[;
  date: string;
  status: standardItem;
  personalDataConsentReceived: str | undefined;
  retentionDelay: str | undefined;
  frenchDisabledWorkerStatus: str | undefined;
}

export interface UploadedFile {
  description: string;
  fileTypeId: string;
  key: string;
}

export interface Applicant {
  personalInformation: PersonalInformation;
  jobPreferences: jobPreferences;
  educations: Education;
  experiences: Experiences;
  consents: dict[;
  furtherInformation: FurtherInformation;
  eEOInformation: eEOInformation;
  customFields: string;
}

export interface TalentsoftApplicantSchema {
  applicant: Applicant;
  application: Application;
  uploadedFiles: UploadedFile[;
}

export interface UpdateSpecialEmploymentRegulationsInFrance {
  disabledWorkerStatus: boolean;
  priorityNeighbourhood: boolean;
}

export interface UpdatePersonalInformation {
  birthDate: string;
  nationalities: str[;
  address: string;
  postalCode: string;
  city: string;
  residentCountryId: string;
  specialEmploymentRegulationsInFrance: 
        UpdateSpecialEmploymentRegulationsInFrance
     | undefined;
}

export interface UpdateJobPreferences {
  primaryProfileId: string;
  dateOfAvailability: string;
  salaryExpectations: string;
}

export interface UpdateLanguage {
  languageId: string;
  languageLevelId: string;
}

export interface UpdateEducation {
  diplomaId: string;
  educationLevelId: string;
}

export interface UpdateExperience {
  experienceLevelId: string;
  profileId: string;
  company: string;
  function: string;
  contractTypeId: string;
}

export interface UpdateAttachment {
  description: string;
  key: string;
  fileType: string;
}

export interface CandidateUpdated {
  employeeNumber: str | undefined;
  lastName: string;
  firstName: string;
  middleName: str | undefined;
  email: string;
  phoneNumber: str | undefined;
  civilityId: str | undefined;
  personalInformation: UpdatePersonalInformation | undefined;
  jobPreferences: UpdateJobPreferences | undefined;
  languages: UpdateLanguage | undefined[;
  educations: UpdateEducation | undefined[;
  experiences: UpdateExperience | undefined[;
  attachments: UpdateAttachment | undefined[;
}

