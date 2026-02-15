/**
 * Greenhouse Warehouse
 * Complete TypeScript translation of greenhouse/warehouse.py
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
import {
  GreenhouseJobModel,
  GreenhouseProfileModel,
} from './schemas';

// Constants
export const GET_JOB_ENDPOINT = new ActionEndpoints({
  name: 'Get job',
  description: 'Endpoint to get the content of a job with a given id. The request method is `GET`',
  url: 'https://developers.greenhouse.io/harvest.html?shell#get-retrieve-job',
});

export const POST_CANDIDATE_ENDPOINT = new ActionEndpoints({
  name: 'Post Candidate',
  description: 'Endpoint to create a new candidate and assign to a talent pool, the request method is `POST`',
  url: 'https://developers.greenhouse.io/job-board.html#jobs',
});

// Parameter Classes
export class WriteProfilesParameters extends ParametersModel {
  auth!: string;
  on_behalf_of!: string;

  constructor(auth: string, on_behalf_of: string) {
    super();
    this.auth = auth;
    this.on_behalf_of = on_behalf_of;
  }

  validate(): void {
    if (!this.auth) {
      throw new Error('auth is required');
    }
    if (!this.on_behalf_of) {
      throw new Error('on_behalf_of is required');
    }
  }
}

export class ReadProfilesParameters extends ParametersModel {
  auth!: string;
  created_after?: string;
  updated_after?: string;
  job_id?: string;
  email?: string;
  candidate_ids?: string;

  constructor(
    auth: string,
    created_after?: string,
    updated_after?: string,
    job_id?: string,
    email?: string,
    candidate_ids?: string
  ) {
    super();
    this.auth = auth;
    this.created_after = created_after;
    this.updated_after = updated_after;
    this.job_id = job_id;
    this.email = email;
    this.candidate_ids = candidate_ids;
  }

  validate(): void {
    if (!this.auth) {
      throw new Error('auth is required');
    }
  }
}

export class ReadJobsParameters extends ParametersModel {
  board_token!: string;

  constructor(board_token: string) {
    super();
    this.board_token = board_token;
  }

  validate(): void {
    if (!this.board_token) {
      throw new Error('board_token is required');
    }
  }
}

// Read functions
export async function* readProfiles(
  adapter: Logger,
  parameters: ReadProfilesParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  try {
    // Prepare auth header
    const authString = parameters.auth + ':';
    const authorization = Buffer.from(authString).toString('base64');

    // Prepare query parameters
    const queryParams: Record<string, string> = {};
    if (parameters.created_after) queryParams.created_after = parameters.created_after;
    if (parameters.updated_after) queryParams.updated_after = parameters.updated_after;
    if (parameters.job_id) queryParams.job_id = parameters.job_id;
    if (parameters.email) queryParams.email = parameters.email;
    if (parameters.candidate_ids) queryParams.candidate_ids = parameters.candidate_ids;

    // Make request
    const response: AxiosResponse = await axios.get(
      'https://harvest.greenhouse.io/v1/candidates',
      {
        headers: {
          Authorization: `Basic ${authorization}`,
        },
        params: queryParams,
      }
    );

    // Check for errors
    if (Math.floor(response.status / 100) !== 2) {
      adapter.error(
        `Failed to pull profiles from Greenhouse params=${JSON.stringify(parameters)} status_code=${response.status} response=${response.data}`
      );
      throw new Error('Failed to pull profiles from Greenhouse');
    }

    // Parse response
    const responseData = response.data;
    const profiles = responseData.candidates || [];
    adapter.info(`Pulling ${profiles.length} profiles`);

    // Yield each profile
    for (const profile of profiles) {
      yield profile;
    }
  } catch (error) {
    adapter.error(`Error in readProfiles: ${error}`);
    throw error;
  }
}

export async function* read(
  adapter: Logger,
  parameters: ReadJobsParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  try {
    while (true) {
      const response: AxiosResponse = await axios.get(
        `https://boards-api.greenhouse.io/v1/boards/${parameters.board_token}/jobs/?content=true`,
        {
          headers: {},
          params: null,
        }
      );

      // Check for errors
      if (Math.floor(response.status / 100) !== 2) {
        adapter.error(
          `Failed to pull jobs from Greenhouse params=${JSON.stringify(parameters)} status_code=${response.status} response=${response.data}`
        );
        throw new Error('Failed to pull jobs from Greenhouse');
      }

      // Parse response
      const responseData = response.data;
      const jobs = responseData.jobs || [];

      if (jobs.length === 0) {
        break;
      }

      adapter.info(`Pulling ${jobs.length} jobs`);
      for (const job of jobs) {
        yield job;
      }
      break;
    }
  } catch (error) {
    adapter.error(`Error in read: ${error}`);
    throw error;
  }
}

export async function write(
  adapter: Logger,
  parameters: WriteProfilesParameters,
  profiles: Array<Record<string, any>>
): Promise<Array<Record<string, any>>> {
  try {
    adapter.info(`Pushing ${profiles.length} profiles`);
    const failedProfiles: Array<Record<string, any>> = [];

    for (const profile of profiles) {
      // Prepare auth header
      const authString = parameters.auth + ':';
      const authorization = Buffer.from(authString).toString('base64');

      // Make request
      const response: AxiosResponse = await axios.post(
        'https://harvest.greenhouse.io/v1/candidates',
        profile,
        {
          headers: {
            'On-Behalf-Of': parameters.on_behalf_of,
            Authorization: `Basic ${authorization}`,
          },
        }
      );

      // Check for errors
      if (Math.floor(response.status / 100) !== 2) {
        adapter.error(
          `Failed to push profile to Greenhouse status_code=${response.status} response=${response.data}`
        );
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
export const GreenhouseJobWarehouse = new Warehouse({
  name: 'Greenhouse Jobs',
  dataSchema: GreenhouseJobModel as any,
  dataType: DataType.job,
  read: new WarehouseReadAction({
    parameters: ReadJobsParameters,
    function: read as any,
    endpoints: [GET_JOB_ENDPOINT],
  }),
});

export const GreenhouseProfileWarehouse = new Warehouse({
  name: 'Greenhouse Profiles',
  dataSchema: GreenhouseProfileModel as any,
  dataType: DataType.profile,
  write: new WarehouseWriteAction({
    parameters: WriteProfilesParameters,
    function: write as any,
    endpoints: [POST_CANDIDATE_ENDPOINT],
  }),
  read: new WarehouseReadAction({
    parameters: ReadProfilesParameters,
    function: readProfiles as any,
  }),
});
