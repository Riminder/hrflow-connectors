/**
 * Workable Warehouse
 * Complete TypeScript translation of workable/warehouse.py
 */

import axios, { AxiosResponse } from 'axios';
import { Logger } from 'pino';
import {
  DataType,
  FieldType,
  ParametersModel,
  ReadMode,
  Warehouse,
  WarehouseReadAction,
  WarehouseWriteAction,
} from '../../core';
import { WorkableJobModel } from './schemas';

// Parameter Classes
export class WorkableReadParameters extends ParametersModel {
  auth!: string;
  subdomain!: string;

  constructor(auth: string, subdomain: string) {
    super();
    this.auth = auth;
    this.subdomain = subdomain;
  }

  validate(): void {
    if (!this.auth) throw new Error('auth is required');
    if (!this.subdomain) throw new Error('subdomain is required');
  }
}

export class WorkableWriteParameters extends ParametersModel {
  auth!: string;
  subdomain!: string;
  shortcode!: string;

  constructor(auth: string, subdomain: string, shortcode: string) {
    super();
    this.auth = auth;
    this.subdomain = subdomain;
    this.shortcode = shortcode;
  }

  validate(): void {
    if (!this.auth) throw new Error('auth is required');
    if (!this.subdomain) throw new Error('subdomain is required');
    if (!this.shortcode) throw new Error('shortcode is required');
  }
}

// Read jobs from Workable API
export async function* read(
  adapter: Logger,
  parameters: WorkableReadParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  try {
    const url = `https://${parameters.subdomain}.workable.com/spi/v3/jobs?state=published`;
    const headers = {
      'Content-Type': 'application/json',
      Authorization: `Bearer ${parameters.auth}`,
    };

    const response = await axios.get(url, { headers });

    if (response.status !== 200) {
      adapter.error(`Failed to read Workable jobs: ${response.statusText}`);
      return;
    }

    const jobs = response.data.jobs || [];
    for (const job of jobs) {
      yield job;
    }
  } catch (error) {
    adapter.error(`Error in read: ${error}`);
    throw error;
  }
}

// Write profiles to Workable API
export async function write(
  adapter: Logger,
  parameters: WorkableWriteParameters,
  profiles: Array<Record<string, any>>
): Promise<Array<Record<string, any>>> {
  try {
    const failedProfiles: Array<Record<string, any>> = [];

    for (const profile of profiles) {
      const url = `https://${parameters.subdomain}.workable.com/spi/v3/jobs/${parameters.shortcode}/candidates`;
      const headers = {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${parameters.auth}`,
        Accept: 'application/json',
      };

      try {
        const response = await axios.post(url, profile, { headers });

        if (response.status !== 200) {
          adapter.error(`Failed to post profile: ${response.statusText}`);
          failedProfiles.push(profile);
        }
      } catch (error) {
        adapter.error(`Error posting profile: ${error}`);
        failedProfiles.push(profile);
      }
    }

    return failedProfiles;
  } catch (error) {
    adapter.error(`Error in write: ${error}`);
    throw error;
  }
}

// Warehouse Definitions
export const WorkableJobWarehouse = new Warehouse({
  name: 'WorkableJobWarehouse',
  dataType: DataType.job,
  read: new WarehouseReadAction({
    parameters: WorkableReadParameters,
    function: read as any,
  }),
});

export const WorkableProfileWarehouse = new Warehouse({
  name: 'WorkableProfileWarehouse',
  dataType: DataType.profile,
  write: new WarehouseWriteAction({
    parameters: WorkableWriteParameters,
    function: write as any,
  }),
});
