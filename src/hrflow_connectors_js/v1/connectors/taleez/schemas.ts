/**
 * Taleez Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Property {
  id: number;
  key: string;
  name: string;
  nameEn: string;
  type: string;
  choices: t.Dict[;
}

export interface Candidate {
  id: number;
  firstName: string;
  lastName: string;
  mail: string;
  initialReferrer: string;
  lang: string;
  socialLinks: dict;
  properties: list | undefined;
  jobs: list | undefined;
}

export interface Job {
  id: number;
  token: string;
  dateCreation: number;
  dateFirstPublish: number;
  dateLastPublish: number;
  label: string;
  currentStatus: string;
  contract: string;
  contractLength: number;
  fullTime: boolean;
  workHours: number;
  remote: boolean;
  country: string;
  city: string;
  postalCode: string;
  lat: string;
  lng: string;
  recruiterId: number;
  who: string;
  logo: string;
  banner: string;
  companyLabel: string;
  tags: t.Dict[;
  url: string;
  urlApplying: string;
  jobDescription: string;
  profileDescription: string;
  companyDescription: string;
  properties: t.Dict[;
  public: boolean;
}

export interface JobProperty {
  key: string;
  value: string;
  values: str[;
}

