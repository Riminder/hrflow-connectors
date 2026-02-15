/**
 * Workable Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Location {
  location_str?: string | undefined;
  country?: string | undefined;
  country_code?: string | undefined;
  region?: string | undefined;
  region_code?: string | undefined;
  city?: string | undefined;
  zip_code?: string | undefined;
  telecommuting?: boolean | undefined;
}

export interface WorkableJobModel {
  id?: string | undefined;
  title: string;
  full_title: string;
  shortcode: string;
  code?: string | undefined;
  state?: string | undefined;
  department?: string | undefined;
  url?: string | undefined;
  application_url?: string | undefined;
  shortlink?: string | undefined;
  location: Location;
  created_at: string;
  description?: string | undefined;
  requirements?: string | undefined;
  benefit?: string | undefined;
  employment_type?: string | undefined;
}

export interface Profile {
  name: string;
  summary?: string | undefined;
  address?: string | undefined;
  headline?: string | undefined;
  phone?: string | undefined;
  email: string;
  resume_url?: string | undefined;
}

export interface WorkableCandidate {
  sourced?: boolean | undefined;
  candidate: Profile;
}

