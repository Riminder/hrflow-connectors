/**
 * Greenhouse Schemas
 * Complete TypeScript translation of greenhouse/schemas.py
 */

// Job Model
export interface Location {
  name: string;
}

export interface Department {
  id: number;
  name: string;
  parent_id: any;
  child_ids?: number[];
}

export interface Office {
  id: number;
  name: string;
  location: string;
  parent_id?: number;
  child_ids?: number[];
}

export interface SalaryRange {
  min_value: number;
  max_value: number;
  unit: string;
}

export interface CustomFields {
  employment_type: string;
  maximum_budget: string;
  salary_range: SalaryRange;
}

export interface EmploymentType {
  name: string;
  type: string;
  value: string;
}

export interface Budget {
  name: string;
  type: string;
  value: string;
}

export interface KeyedCustomFields {
  employement_type: EmploymentType;
  budget: Budget;
  salary_range: SalaryRange;
}

export interface TeamMember {
  id: number;
  first_name: string;
  last_name: string;
  name: string;
  employee_id: string;
  responsible?: boolean;
}

export interface HiringTeam {
  hiring_managers: TeamMember[];
  recruiters: TeamMember[];
  coordinators: TeamMember[];
  sourcers: TeamMember[];
}

export interface CloseReason {
  id: number;
  name: string;
}

export interface Opening {
  id: number;
  opening_id: string;
  status: string;
  opened_at: string;
  closed_at: string;
  application_id: number;
  close_reason: CloseReason;
}

export interface GreenhouseJobModel {
  id: number;
  internal_job_id: number;
  title: string;
  updated_at?: string;
  requisition_id?: string;
  location: Location;
  absolute_url: string;
  metadata: any;
  content: string;
  departments: Department[];
  offices: Office[];
}

// Profile Model
export interface PhoneNumber {
  value: string;
  type: string;
}

export interface Address {
  value: string;
  type: string;
}

export interface EmailAddress {
  value: string;
  type: string;
}

export interface WebsiteAddress {
  value: string;
  type: string;
}

export interface SocialMediaAddress {
  value: string;
}

export interface Education {
  school_id: number;
  discipline_id: number;
  degree_id: number;
  start_date: string;
  end_date: string;
}

export interface Employment {
  company_name: string;
  title: string;
  start_date: string;
  end_date: string;
}

export interface Recruiter {
  id?: number;
  email?: string;
}

export interface Coordinator {
  id: number;
  email: string;
}

export interface GreenhouseProfileModel {
  first_name: string;
  last_name: string;
  company?: string;
  title?: string;
  phone_numbers?: PhoneNumber[];
  addresses?: Address[];
  email_addresses?: EmailAddress[];
  website_addresses?: WebsiteAddress[];
  social_media_addresses?: SocialMediaAddress[];
  educations?: Education[];
  employments?: Employment[];
  tags?: string[];
  applications: Array<{ job_id: number }>;
  recruiter?: Recruiter;
  coordinator?: Coordinator;
}
