/**
 * Carrevolutis Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface CarrevolutisEventObject {
  type: string;
  jobkey: str | undefined;
  firstName: str | undefined;
  lastName: str | undefined;
  phone: str | undefined;
  email: string;
  cvBase64: string;
  coverText: str | undefined;
  profilecountry: str | undefined;
  profileregions: str | undefined;
  profiledomains: str | undefined;
  joblien_annonce_site_carriere: str | undefined;
  statisticsource: str | undefined;
  statisticjbsource: str | undefined;
}

