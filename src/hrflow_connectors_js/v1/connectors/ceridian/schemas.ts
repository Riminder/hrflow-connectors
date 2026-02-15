/**
 * Ceridian Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface CeridianDayforceJobModel {
  Title: string;
  Description?: string | undefined;
  ClientSiteName?: string | undefined;
  ClientSiteXRefCode?: string | undefined;
  CompanyName?: string | undefined;
  ParentCompanyName?: string | undefined;
  JobDetailsUrl: string;
  ApplyUrl?: string | undefined;
  AddressLine1?: string | undefined;
  City?: string | undefined;
  State?: string | undefined;
  Country?: string | undefined;
  PostalCode?: string | undefined;
  DatePosted?: string | undefined;
  LastUpdated?: string | undefined;
  ReferenceNumber: number;
  ParentRequisitionCode?: number | undefined;
  IsVirtualLocation?: boolean | undefined;
}

