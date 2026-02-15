/**
 * SmartRecruiters Warehouse
 * Complete TypeScript translation of smartrecruiters/warehouse.py
 */

import axios from 'axios';
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
import { SmartRecruitersJob, SmartRecruitersProfile } from './schemas';

const SMARTRECRUITERS_JOBS_ENDPOINT = 'https://api.smartrecruiters.com/jobs';
const SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT = 100;

// Enums
export enum JobPostingStatus {
  public = 'PUBLIC',
  internal = 'INTERNAL',
  not_published = 'NOT_PUBLISHED',
  private = 'PRIVATE',
}

export enum JobStatus {
  created = 'CREATED',
  sourcing = 'SOURCING',
  filled = 'FILLED',
  interview = 'INTERVIEW',
  offer = 'OFFER',
  cancelled = 'CANCELLED',
  on_hold = 'ON_HOLD',
}

// Parameter Classes
export class WriteProfilesParameters extends ParametersModel {
  x_smart_token!: string;
  job_id!: string;

  constructor(x_smart_token: string, job_id: string) {
    super();
    this.x_smart_token = x_smart_token;
    this.job_id = job_id;
  }

  validate(): void {
    if (!this.x_smart_token) throw new Error('x_smart_token is required');
    if (!this.job_id) throw new Error('job_id is required');
  }
}

export class ReadJobsParameters extends ParametersModel {
  x_smart_token!: string;
  query?: string;
  updated_after?: string;
  posting_status?: JobPostingStatus;
  job_status?: JobStatus;
  limit!: number;

  constructor(
    x_smart_token: string,
    query?: string,
    updated_after?: string,
    posting_status?: JobPostingStatus,
    job_status?: JobStatus,
    limit: number = SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT
  ) {
    super();
    this.x_smart_token = x_smart_token;
    this.query = query;
    this.updated_after = updated_after;
    this.posting_status = posting_status;
    this.job_status = job_status;
    this.limit = limit;
  }

  validate(): void {
    if (!this.x_smart_token) throw new Error('x_smart_token is required');
  }
}

// Read jobs from SmartRecruiters API
export async function* read(
  adapter: Logger,
  parameters: ReadJobsParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  let page: string | null = null;

  while (true) {
    const params: Record<string, any> = {
      q: parameters.query,
      updatedAfter: parameters.updated_after,
      postingStatus: parameters.posting_status,
      status: parameters.job_status,
      limit: Math.min(parameters.limit, SMARTRECRUITERS_JOBS_ENDPOINT_LIMIT),
      pageId: page,
    };

    try {
      const response = await axios.get(SMARTRECRUITERS_JOBS_ENDPOINT, {
        headers: { 'X-SmartToken': parameters.x_smart_token },
        params,
      });

      if (response.status < 200 || response.status >= 300) {
        adapter.error(
          `Failed to pull jobs from SmartRecruiters params=${JSON.stringify(params)} status_code=${response.status} response=${response.data}`
        );
        throw new Error('Failed to pull jobs from SmartRecruiters');
      }

      const responseData = response.data;
      const jobs = responseData.content || [];

      if (jobs.length === 0) {
        break;
      }

      adapter.info(
        `Pulling ${jobs.length} jobs from page ${page || 1} out of total jobs ${responseData.totalFound}`
      );

      for (const job of jobs) {
        try {
          const fullJobResponse = await axios.get(
            `${SMARTRECRUITERS_JOBS_ENDPOINT}/${job.id}`,
            {
              headers: { 'X-SmartToken': parameters.x_smart_token },
            }
          );

          if (fullJobResponse.status < 200 || fullJobResponse.status >= 300) {
            adapter.error(
              `Failed to pull job details from SmartRecruiters job_id=${job.id} status_code=${fullJobResponse.status} response=${fullJobResponse.data}`
            );
            throw new Error('Failed to pull job details from SmartRecruiters');
          }

          yield fullJobResponse.data;
        } catch (error) {
          adapter.error(`Error fetching job ${job.id}: ${error}`);
          throw error;
        }
      }

      page = responseData.nextPageId;
      if (!page) {
        break;
      }
    } catch (error) {
      adapter.error(`Error in read: ${error}`);
      throw error;
    }
  }
}

// Write profiles to SmartRecruiters API
export async function write(
  adapter: Logger,
  parameters: WriteProfilesParameters,
  profiles: Array<Record<string, any>>
): Promise<Array<Record<string, any>>> {
  adapter.info(
    `Pushing ${profiles.length} profiles with job_id=${parameters.job_id}`
  );

  const failedProfiles: Array<Record<string, any>> = [];

  for (const profile of profiles) {
    try {
      const response = await axios.post(
        `${SMARTRECRUITERS_JOBS_ENDPOINT}/${parameters.job_id}/candidates`,
        profile,
        {
          headers: { 'X-SmartToken': parameters.x_smart_token },
        }
      );

      if (response.status < 200 || response.status >= 300) {
        adapter.error(
          `Failed to push profile to SmartRecruiters job_id=${parameters.job_id} status_code=${response.status} response=${response.data}`
        );
        failedProfiles.push(profile);
      }
    } catch (error) {
      adapter.error(`Error pushing profile: ${error}`);
      failedProfiles.push(profile);
    }
  }

  return failedProfiles;
}

// Warehouse Definitions
export const SmartRecruitersJobWarehouse = new Warehouse({
  name: 'SmartRecruiters Jobs',
  dataType: DataType.job,
  read: new WarehouseReadAction({
    parameters: ReadJobsParameters,
    function: read as any,
  }),
});

export const SmartRecruitersProfileWarehouse = new Warehouse({
  name: 'SmartRecruiters Profiles',
  dataType: DataType.profile,
  write: new WarehouseWriteAction({
    parameters: WriteProfilesParameters,
    function: write as any,
  }),
});
