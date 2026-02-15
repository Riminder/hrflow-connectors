/**
 * Breezyhr Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Type {
  id: string;
  name: string;
}

export interface Experience {
  id: string;
  name: string;
}

export interface Country {
  name: string;
  id: string;
}

export interface Location {
  country: Country;
  city: string;
  is_remote?: boolean | undefined;
  name: string;
}

export interface Category {
  id: string;
  name: string;
}

export interface ApplicationForm {
  name: string;
  headline: string;
  summary: string;
  profile_photo: string;
  address: string;
  email_address: string;
  phone_number: string;
  resume: string;
  work_history: string;
  education: string;
  cover_letter: string;
  questionnaire_in_experience: boolean;
}

export interface BreezyJobModel {
  _id: string;
  type: Type;
  state: string;
  name: string;
  friendly_id: string;
  experience?: Experience | undefined;
  location: Location;
  education: string;
  department: string;
  description: string;
  category: Category;
  application_form?: ApplicationForm | undefined;
  creator_id?: string | undefined;
  creation_date: string;
  updated_date: string;
  all_users: string[;
  all_admins: string[;
  candidate_type: string;
  tags: List;
  org_type: string;
}

export interface WorkHistoryItem {
  company_name: string;
  title: string;
  summary: string;
  start_month?: number | undefined;
  start_year?: number | undefined;
  end_month?: number | undefined;
  end_year?: number | undefined;
}

export interface EducationItem {
  school_name: string;
  field_of_study: string;
  start_year?: number | undefined;
  end_year?: number | undefined;
}

export interface BreezyProfileModel {
  name: string;
  email_address: string;
  phone_number: string;
  summary: string;
  tags?: string[] | undefined;
  source?: string | undefined;
  origin?: string | undefined;
  address: string;
  work_history: WorkHistoryItem[;
  education: EducationItem[;
  social_profiles?: string[] | undefined;
  custom_attributes?: Record<string, any>[] | undefined;
  cover_letter?: string | undefined;
}

