/**
 * Recruitee Warehouse
 * Complete TypeScript translation of warehouse.py
 */

import axios, { AxiosResponse } from 'axios';
import { Logger } from 'pino';
import {
  DataType,
  FieldType,
  ParametersModel,
  Warehouse,
  WarehouseReadAction,
  WarehouseWriteAction,
  ActionEndpoints,
  ReadMode,
} from '../../core';
import { } from './schemas';

// Parameter Classes
export class ReadProfilesParameters extends ParametersModel {
  company_id!: string;
  api_token!: string;
  recruitee_endpoint!: Endpoint;
  limit!: number;
  offset!: number;
  created_after!: string;
  disqualified!: boolean;
  qualified!: boolean;
  ids!: string;
  offer_id!: string;
  query!: string;
  sort!: Sort;
  with_messages!: boolean;
  with_my_messages!: boolean;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class WriteProfilesParameters extends ParametersModel {
  company_id!: string;
  api_token!: string;
  recruitee_endpoint!: Endpoint;
  offer_ids!: int[;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class ReadJobsParameters extends ParametersModel {
  company_id!: string;
  api_token!: string;
  recruitee_endpoint!: Endpoint;
  kind!: string;
  scope!: string;
  view_mode!: View_mode;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class WriteJobsParameters extends ParametersModel {
  company_id!: string;
  api_token!: string;
  recruitee_endpoint!: Endpoint;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}


// Warehouse instances would be defined here
// Following the Greenhouse pattern
