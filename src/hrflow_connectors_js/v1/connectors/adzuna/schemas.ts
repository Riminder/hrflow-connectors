/**
 * Adzuna Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface Category {
  __CLASS__: string;
  tag: string;
  label: string;
}

export interface Location {
  __CLASS__: string;
  area: str[;
  display_name: string;
}

export interface Company {
  __CLASS__: string;
  display_name: string;
  canonical_name: str | undefined;
  count: int | undefined;
}

export interface AdzunaJob {
  id: string;
  created: string;
  title: string;
  description: string;
  full_description: str | undefined;
  redirect_url: string;
  latitude: float | undefined;
  longitude: float | undefined;
  category: Category;
  location: Location;
  salary_min: number;
  salary_max: number;
  salary_is_predicted: Flag;
  company: Company;
  contract_type: ContractType | undefined;
  contract_time: ContractTime;
}

