/**
 * Sapsuccessfactors Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface SAPSuccessFactorsJobRequistion {
  annual_SA?: string | undefined;
  location?: string | undefined;
  city?: string | undefined;
  country?: string | undefined;
  department?: string | undefined;
  division?: string | undefined;
  facility?: string | undefined;
  function?: string | undefined;
  industry?: string | undefined;
  monthly_salary?: string | undefined;
  salaryBase?: string | undefined;
  otherBonus?: string | undefined;
  salaryMax?: string | undefined;
  salaryMin?: string | undefined;
  stateProvince?: string | undefined;
  jobStartDate?: string | undefined;
  recruiterTeam?: Record<string, any> | undefined;
  hiringManagerTeam?: Record<string, any> | undefined;
  sourcerTeam?: Record<string, any> | undefined;
}

export interface SAPSuccessFactorsJob {
  jobDescription?: string | undefined;
  jobTitle?: string | undefined;
  jobReqId?: string | undefined;
  jobRequisition: SAPSuccessFactorsJobRequistion;
}

export interface Result {
  endDate?: string | undefined;
  school: string;
  schoolAddress: string;
  startDate?: string | undefined;
}

export interface Education {
  results: Result[;
}

export interface ResultLanguage {
  language: string;
  readingProf: string;
  speakingProf: string;
  writingProf: string;
}

export interface ResultOutsideWorkExperience {
  employer?: string | undefined;
  employerAddress: string;
  endDate?: string | undefined;
  startDate?: string | undefined;
}

export interface OutsideWorkExperience {
  results: ResultOutsideWorkExperience[;
}

export interface InsideWorkExperienceResult {
  backgroundElementId?: string | undefined;
  bgOrderPos?: string | undefined;
  candidateId?: string | undefined;
  department?: string | undefined;
  endDate?: string | undefined;
  lastModifiedDateTime?: string | undefined;
  startDate?: string | undefined;
  title?: string | undefined;
  candidate?: string | undefined;
}

export interface TalentPoolResults {
  startDate?: string | undefined;
  talentPoolComments?: string | undefined;
  talentPoolStatus?: string | undefined;
  talentPoolitem?: string | undefined;
}

export interface TalentPool {
  results: TalentPoolResults[;
}

export interface SapCandidateModel {
  address?: string | undefined;
  cellPhone?: string | undefined;
  city?: string | undefined;
  contactEmail?: string | undefined;
  country?: string | undefined;
  currentTitle?: string | undefined;
  firstName?: string | undefined;
  homePhone?: string | undefined;
  lastName?: string | undefined;
  middleName?: string | undefined;
  primaryEmail: string;
  zip?: string | undefined;
  education?: Education | undefined;
  outsideWorkExperience?: OutsideWorkExperience | undefined;
}

