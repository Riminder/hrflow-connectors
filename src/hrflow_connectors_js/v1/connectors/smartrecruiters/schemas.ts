/**
 * SmartRecruiters Schemas
 * Complete TypeScript translation of smartrecruiters/schemas.py
 */

export interface Department {
  id: string;
}

export interface JobLocation {
  country?: string;
  countryCode?: string;
  regionCode?: string;
  region?: string;
  city: string;
  address?: string;
  longitude?: string;
  latitude?: string;
  remote?: boolean;
  manual?: boolean;
}

export interface Industry {
  id: string;
}

export interface Function {
  id: string;
}

export interface TypeOfEmployment {
  id: string;
}

export interface ExperienceLevel {
  id?: string;
}

export interface EeoCategory {
  id: string;
}

export interface Creator {
  firstName: string;
  lastName: string;
}

export interface Compensation {
  min: number;
  max: number;
  currency: string;
}

export interface CompanyDescription {
  title: string;
  text: string;
}

export interface JobDescription {
  title: string;
  text: string;
}

export interface Qualifications {
  title: string;
  text: string;
}

export interface AdditionalInformation {
  title: string;
  text: string;
}

export interface Sections {
  companyDescription: CompanyDescription;
  jobDescription: JobDescription;
  qualifications: Qualifications;
  additionalInformation: AdditionalInformation;
}

export interface JobAd {
  sections: Sections;
}

export interface SmartRecruitersJob {
  title: string;
  refNumber: string;
  createdOn: string;
  updatedOn: string;
  department?: Department;
  location: JobLocation;
  status?: string;
  postingStatus?: string;
  targetHiringDate?: string;
  industry?: Industry;
  function?: Function;
  typeOfEmployment?: TypeOfEmployment;
  experienceLevel?: ExperienceLevel;
  eeoCategory?: EeoCategory;
  creator?: Creator;
  compensation?: Compensation;
  jobAd: JobAd;
}

export interface ProfileLocation {
  country: string;
  countryCode: string;
  regionCode: string;
  region: string;
  city: string;
  lat: number;
  lng: number;
}

export interface Web {
  skype: string;
  linkedin: string;
  facebook: string;
  twitter: string;
  website: string;
}

export interface EducationItem {
  institution: string;
  degree: string;
  major: string;
  current: boolean;
  location: string;
  startDate: string;
  endDate: string;
  description: string;
}

export interface ExperienceItem {
  title: string;
  company: string;
  current: boolean;
  startDate: string;
  endDate: string;
  location: string;
  description: string;
}

export interface SmartRecruitersProfile {
  firstName: string;
  lastName: string;
  email: string;
  phoneNumber: string;
  location: ProfileLocation;
  web: Web;
  tags: Array<string>;
  education: Array<EducationItem>;
  experience: Array<ExperienceItem>;
}
