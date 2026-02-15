/**
 * Recruitee Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface RecruiteeProfile {
  name: string;
  remote_cv_url?: string | undefined;
  emails?: string[] | undefined;
  phones?: string[] | undefined;
  social_links?: string[] | undefined;
  links?: string[] | undefined;
  cover_letter?: string | undefined;
  sources?: string[] | undefined;
}

export interface RecruiteeJob {
  title: string;
  department?: string | undefined;
  kind?: Kind | undefined;
  description: string;
  requirements: string;
  postal_code: string;
  city: string;
  state_code: string;
  country_code: string;
}

