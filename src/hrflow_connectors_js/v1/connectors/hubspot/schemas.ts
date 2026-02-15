/**
 * Hubspot Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Properties {
  email: string;
  firstname: string;
  lastname: string;
  date_of_birth: str | undefined;
  phone: str | undefined;
  address: str | undefined;
  zip: str | undefined;
  city: str | undefined;
  state: str | undefined;
  country: str | undefined;
  jobtitle: str | undefined;
  company: str | undefined;
  annualrevenue: str | undefined;
  website: str | undefined;
}

export interface ContactObject {
  properties: Properties;
}

