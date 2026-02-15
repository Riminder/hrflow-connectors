/**
 * Hrflow Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Location {
  text: str | undefined;
  lat: float | undefined;
  lng: float | undefined;
  _fields: Record<string, any>;
}

export interface GeneralEntitySchema {
  name: string;
  value: str | undefined;
}

export interface Skill {
  name: string;
  type: t.Literal["hard", "soft";
  value: str | undefined;
}

export interface Label {
  board_key: string;
  job_key: string;
  job_reference: string;
  stage: t.Literal["yes", "no", "later";
  date_stage: string;
  rating: t.Literal[1, 2, 3, 4, 5 | undefined;
  date_rating: string;
}

export interface Section {
  name: str | undefined;
  title: str | undefined;
  description: str | undefined;
}

export interface RangesFloat {
  name: str | undefined;
  value_min: float | undefined;
  value_max: float | undefined;
  unit: str | undefined;
}

export interface RangesDate {
  name: str | undefined;
  value_min: str | undefined;
  value_max: str | undefined;
}

export interface HrFlowJob {
  key: str | undefined;
  reference: str | undefined;
  name: string;
  location: Location;
  sections: Section[;
  url: str | undefined;
  summary: str | undefined;
  archieved_at: str | undefined;
  updated_at: str | undefined;
  created_at: str | undefined;
  skills: Skill | undefined[;
  languages: GeneralEntitySchema | undefined[;
  certifications: GeneralEntitySchema | undefined[;
  courses: GeneralEntitySchema | undefined[;
  tasks: GeneralEntitySchema | undefined[;
  tags: GeneralEntitySchema | undefined[;
  metadatas: GeneralEntitySchema | undefined[;
  ranges_float: RangesFloat | undefined[;
  ranges_date: RangesDate | undefined[;
}

export interface InfoUrl {
  type: t.Literal["from_resume", "linkedin", "twitter", "facebook", "github";
  url: str | undefined;
}

export interface ProfileInfo {
  full_name: str | undefined;
  first_name: str | undefined;
  last_name: str | undefined;
  email: str | undefined;
  phone: str | undefined;
  date_birth: str | undefined;
  location: Location | undefined;
  urls: InfoUrl | undefined[;
  picture: str | undefined;
  gender: str | undefined;
  summary: str | undefined;
}

export interface Experience {
  key: str | undefined;
  company: str | undefined;
  logo: str | undefined;
  title: str | undefined;
  description: str | undefined;
  location: Location | undefined;
  date_start: str | undefined;
  date_end: str | undefined;
  skills: Skill | undefined[;
  certifications: GeneralEntitySchema | undefined[;
  courses: GeneralEntitySchema | undefined[;
  tasks: GeneralEntitySchema | undefined[;
}

export interface Education {
  key: str | undefined;
  school: str | undefined;
  logo: str | undefined;
  title: str | undefined;
  description: str | undefined;
  location: Location | undefined;
  date_start: str | undefined;
  date_end: str | undefined;
  skills: Skill | undefined[;
  certifications: GeneralEntitySchema | undefined[;
  courses: GeneralEntitySchema | undefined[;
  tasks: GeneralEntitySchema | undefined[;
}

export interface HrFlowProfile {
  key: str | undefined;
  reference: str | undefined;
  info: ProfileInfo;
  text_language: string;
  text: string;
  archived_at: str | undefined;
  updated_at: str | undefined;
  created_at: str | undefined;
  experiences_duration: number;
  educations_duration: number;
  experiences: Experience | undefined[;
  educations: Education | undefined[;
  attachments: t.List;
  skills: Skill | undefined[;
  languages: GeneralEntitySchema | undefined[;
  certifications: GeneralEntitySchema | undefined[;
  courses: GeneralEntitySchema | undefined[;
  tasks: GeneralEntitySchema | undefined[;
  interests: GeneralEntitySchema | undefined[;
  tags: GeneralEntitySchema | undefined[;
  metadatas: GeneralEntitySchema | undefined[;
  labels: Label | undefined[;
}

export interface ResumeToParse {
  raw: bytes;
  content_type: string;
}

export interface HrFlowProfileParsing {
  reference: str | undefined;
  created_at: string;
  resume: ResumeToParse;
  tags: GeneralEntitySchema[;
  metadatas: GeneralEntitySchema[;
}

