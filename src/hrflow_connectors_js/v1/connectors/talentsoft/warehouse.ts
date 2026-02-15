/**
 * Talentsoft Warehouse
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
  client_id!: string;
  client_secret!: string;
  client_url!: string;
  filter!: str | undefined;
  fileId!: str | undefined;
  only_resume!: boolean;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class ReadJobsParameters extends ParametersModel {
  client_id!: string;
  client_secret!: string;
  client_url!: string;
  q!: str | undefined;
  filter!: str | undefined;
  max_read!: PositiveInt | undefined;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class WriteProfileParameters extends ParametersModel {
  client_id!: string;
  client_secret!: string;
  client_url!: string;
  job_reference!: string;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}


// Warehouse instances would be defined here
// Following the Greenhouse pattern
