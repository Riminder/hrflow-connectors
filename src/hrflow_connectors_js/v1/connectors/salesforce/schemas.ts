/**
 * Salesforce Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Attributes {
  type: string;
  url: string;
}

export interface SalesforceRecord {
  attributes: Attributes;
}

export interface GeneralEntitySchema {
  name: string;
  value: str | undefined;
}

export interface Skill {
  name: string;
  type: str | undefined;
  value: str | undefined;
}

export interface URLs {
  from_resume: str | undefined[;
  linkedin: str | undefined;
  twitter: str | undefined;
  facebook: str | undefined;
  github: str | undefined;
}

export interface Section {
  name: string;
  title: string;
  description: string;
}

export interface RangeFloat {
  name: string;
  value_min: number;
  value_max: number;
  unit: string;
}

export interface RangeDate {
  name: string;
  value_min: string;
  value_max: string;
}

