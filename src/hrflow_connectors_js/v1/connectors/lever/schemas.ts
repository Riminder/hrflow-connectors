/**
 * Lever Schemas
 * Complete TypeScript translation of lever/schemas.py
 */

export interface LeverJobContent {
  description: string;
  descriptionHtml: string;
  lists: Array<Record<string, string>>;
  closing: string;
  closingHtml: string;
}

export interface LeverJobCategories {
  commitment: string;
  department: string;
  level: string;
  location: string;
  team: string;
}

export interface LeverJobUrls {
  list: string;
  show: string;
  apply: string;
}

export interface LeverJobSalaryRange {
  min: number;
  max: number;
  currency: string;
  interval: string;
}

export interface LeverJob {
  id: string;
  text: string;
  state: string;
  distributionChannels: string[];
  user: string;
  owner: string;
  hiringManager: string;
  categories: LeverJobCategories;
  tags: string[];
  content: LeverJobContent;
  country: string;
  followers: string[];
  reqCode: string;
  requisitionCodes: string[];
  urls: LeverJobUrls;
  confidentiality: string;
  createdAt: number;
  updatedAt: number;
  workplaceType: string;
  salaryRange: LeverJobSalaryRange;
}

export interface LeverProfile {
  id: string;
  name: Record<string, string>;
  email?: string;
  phone?: string;
  createdAt: string;
  updatedAt: string;
}

