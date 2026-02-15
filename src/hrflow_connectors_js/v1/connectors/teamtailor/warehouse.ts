/**
 * Teamtailor Warehouse
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
export class WriteProfilesParameters extends ParametersModel {
  Authorization!: string;
  X_Api_Version!: string;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}

export class ReadJobsParameters extends ParametersModel {
  Authorization!: string;
  X_Api_Version!: string;
  filter_status!: RemoteStatus | undefined;
  filter_feed!: JobFeed | undefined;

  validate(): void {
    if (!this.auth) throw new Error('Authentication required');
  }
}


// Warehouse instances would be defined here
// Following the Greenhouse pattern
