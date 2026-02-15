/**
 * Lever Warehouse
 * Complete TypeScript translation of lever/warehouse.py
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
import { LeverJob, LeverProfile } from './schemas';

// Constants
const LEVER_AUTH_ENDPOINT = 'https://{auth_domain}.auth0.com';
const LEVER_REDIRECT_URI = 'https://marketplace-partners.hrflow.ai/partner/lever/login';
const LEVER_BASE_URL = 'https://{client_domain}.lever.co';
const LEVER_JOBS_ENDPOINT = 'https://{client_domain}.lever.co/v1/postings';
const LEVER_OPPORTUNITIES_ENDPOINT = 'https://{client_domain}.lever.co/v1/opportunities';

export const GET_ALL_JOBS_ENDPOINT = new ActionEndpoints({
  name: 'Get all jobs',
  description: 'Endpoint to get the list of all jobs',
  url: LEVER_JOBS_ENDPOINT,
});

export const GET_ALL_PROFILES_ENDPOINT = new ActionEndpoints({
  name: 'Get all profiles',
  description: 'Endpoint to get the list of all profiles',
  url: LEVER_OPPORTUNITIES_ENDPOINT,
});

export const POST_PROFILE_ENDPOINT = new ActionEndpoints({
  name: 'Post Profile',
  description: 'Endpoint to create a new profile',
  url: LEVER_OPPORTUNITIES_ENDPOINT,
});

// Parameter Classes
export class LeverParameters extends ParametersModel {
  auth_domain!: string;
  client_domain!: string;
  client_id!: string;
  client_secret!: string;
  authorization_code!: string;

  constructor(
    auth_domain: string,
    client_domain: string,
    client_id: string,
    client_secret: string,
    authorization_code: string
  ) {
    super();
    this.auth_domain = auth_domain;
    this.client_domain = client_domain;
    this.client_id = client_id;
    this.client_secret = client_secret;
    this.authorization_code = authorization_code;
  }

  validate(): void {
    if (!this.auth_domain) throw new Error('auth_domain is required');
    if (!this.client_domain) throw new Error('client_domain is required');
    if (!this.client_id) throw new Error('client_id is required');
    if (!this.client_secret) throw new Error('client_secret is required');
    if (!this.authorization_code) throw new Error('authorization_code is required');
  }
}

export class ReadParameters extends LeverParameters {
  limit: number = 100;

  constructor(
    auth_domain: string,
    client_domain: string,
    client_id: string,
    client_secret: string,
    authorization_code: string,
    limit: number = 100
  ) {
    super(auth_domain, client_domain, client_id, client_secret, authorization_code);
    this.limit = limit;
  }
}

export class ReadJobsParameters extends ReadParameters {}

export class WriteProfileParameters extends LeverParameters {
  perform_as!: string;
  parse: boolean = false;
  perform_as_posting_owner: boolean = false;

  constructor(
    auth_domain: string,
    client_domain: string,
    client_id: string,
    client_secret: string,
    authorization_code: string,
    perform_as: string,
    parse: boolean = false,
    perform_as_posting_owner: boolean = false
  ) {
    super(auth_domain, client_domain, client_id, client_secret, authorization_code);
    this.perform_as = perform_as;
    this.parse = parse;
    this.perform_as_posting_owner = perform_as_posting_owner;
  }
}

export class ReadProfilesParameters extends ReadParameters {}

// Helper function to get or refresh OAuth tokens
async function getOrRefreshTokens(
  authDomain: string,
  clientId: string,
  clientSecret: string,
  flow: 'authorization_code' | 'refresh_token',
  code: string
): Promise<[string, string]> {
  const leverAuthEndpoint = LEVER_AUTH_ENDPOINT.replace('{auth_domain}', authDomain);
  const url = `${leverAuthEndpoint}/oauth/token`;
  const redirectUri = LEVER_REDIRECT_URI;

  const requestData: Record<string, string> = {
    client_id: clientId,
    client_secret: clientSecret,
    grant_type: flow,
  };

  if (flow === 'authorization_code') {
    requestData.code = code;
    requestData.redirect_uri = redirectUri;
  } else {
    requestData.refresh_token = code;
  }

  try {
    const response = await axios.post(url, requestData);
    if (response.status === 200) {
      const responseData = response.data;
      const accessToken = responseData.access_token;
      const newRefreshToken = responseData.refresh_token;
      return [accessToken, newRefreshToken];
    }
    throw new Error(
      `Failed to obtain token. Status code: ${response.status}, Response: ${JSON.stringify(response.data)}`
    );
  } catch (error) {
    throw error;
  }
}

// Read jobs from Lever API
export async function* readJobs(
  adapter: Logger,
  parameters: ReadJobsParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  try {
    const [token, refreshToken] = await getOrRefreshTokens(
      parameters.auth_domain,
      parameters.client_id,
      parameters.client_secret,
      'authorization_code',
      parameters.authorization_code
    );

    const jobsUrl = LEVER_JOBS_ENDPOINT.replace('{client_domain}', parameters.client_domain);
    let offset: string | null = null;
    let currentRefreshToken = refreshToken;
    let currentAccessToken = token;

    while (true) {
      try {
        const headers = { Authorization: `Bearer ${currentAccessToken}` };
        const requestParams: Record<string, any> = { limit: parameters.limit };
        if (offset) {
          requestParams.offset = offset;
        }

        const response = await axios.get(jobsUrl, { headers, params: requestParams });

        if (response.status === 200) {
          const jobs = response.data.data || [];
          for (const job of jobs) {
            yield job;
          }

          if (response.data.hasNext) {
            offset = response.data.next || null;
          } else {
            break;
          }
        }
      } catch (error: any) {
        if (error.response?.status === 429) {
          adapter.warn('Rate limit exceeded. Retrying after 5 seconds.');
          await new Promise((resolve) => setTimeout(resolve, 5000));
        } else if (error.response?.status === 401) {
          adapter.warn('Access token has expired. Refreshing token and retrying.');
          const [newToken, newRefreshToken] = await getOrRefreshTokens(
            parameters.auth_domain,
            parameters.client_id,
            parameters.client_secret,
            'refresh_token',
            currentRefreshToken
          );
          currentAccessToken = newToken;
          currentRefreshToken = newRefreshToken;
        } else {
          throw new Error(
            `Failed to retrieve jobs from Lever. Status code: ${error.response?.status}, Response: ${error.message}`
          );
        }
      }
    }
  } catch (error) {
    adapter.error(`Error in readJobs: ${error}`);
    throw error;
  }
}

// Read profiles from Lever API
export async function* readProfiles(
  adapter: Logger,
  parameters: ReadProfilesParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  try {
    let [token, refreshToken] = await getOrRefreshTokens(
      parameters.auth_domain,
      parameters.client_id,
      parameters.client_secret,
      'authorization_code',
      parameters.authorization_code
    );

    const profilesUrl = LEVER_OPPORTUNITIES_ENDPOINT.replace(
      '{client_domain}',
      parameters.client_domain
    );
    let offset: string | null = null;
    let currentRefreshToken = refreshToken;
    let currentAccessToken = token;

    while (true) {
      try {
        const headers = { Authorization: `Bearer ${currentAccessToken}` };
        const requestParams: Record<string, any> = { limit: parameters.limit };
        if (offset) {
          requestParams.offset = offset;
        }

        const response = await axios.get(profilesUrl, { headers, params: requestParams });

        if (response.status === 200) {
          const opportunities = response.data.data || [];

          for (const opportunity of opportunities) {
            const opportunityId = opportunity.id;
            try {
              const profileResponse = await axios.get(`${profilesUrl}/${opportunityId}/resumes`, {
                headers,
              });

              if (profileResponse.status === 200) {
                const profileData = profileResponse.data;
                opportunity.profile = profileData.data;
                yield opportunity;
              }
            } catch (error: any) {
              throw new Error(
                `Failed to retrieve profiles for opportunity ${opportunityId}. Status code: ${error.response?.status}, Response: ${error.message}`
              );
            }
          }

          if (response.data.hasNext) {
            offset = response.data.next || null;
          } else {
            break;
          }
        }
      } catch (error: any) {
        if (error.response?.status === 429) {
          adapter.warn('Rate limit exceeded. Retrying after 5 seconds.');
          await new Promise((resolve) => setTimeout(resolve, 5000));
        } else if (error.response?.status === 401) {
          adapter.warn('Access token has expired. Refreshing token and retrying.');
          const [newToken, newRefreshToken] = await getOrRefreshTokens(
            parameters.auth_domain,
            parameters.client_id,
            parameters.client_secret,
            'refresh_token',
            currentRefreshToken
          );
          currentAccessToken = newToken;
          currentRefreshToken = newRefreshToken;
        } else {
          throw error;
        }
      }
    }
  } catch (error) {
    adapter.error(`Error in readProfiles: ${error}`);
    throw error;
  }
}

// Write profiles to Lever API
export async function write(
  adapter: Logger,
  parameters: WriteProfileParameters,
  profiles: Array<Record<string, any>>
): Promise<Array<Record<string, any>>> {
  try {
    let [token, refreshToken] = await getOrRefreshTokens(
      parameters.auth_domain,
      parameters.client_id,
      parameters.client_secret,
      'authorization_code',
      parameters.authorization_code
    );

    const urlPostOpportunity = LEVER_OPPORTUNITIES_ENDPOINT.replace(
      '{client_domain}',
      parameters.client_domain
    );
    const failedProfiles: Array<Record<string, any>> = [];
    const requestParams = {
      perform_as: parameters.perform_as,
      parse: parameters.parse,
      perform_as_posting_owner: parameters.perform_as_posting_owner,
    };

    let i = 0;
    let currentRefreshToken = refreshToken;
    let currentAccessToken = token;

    while (i < profiles.length) {
      const profile = { ...profiles[i] };
      delete profile.file;

      try {
        const headers = { Authorization: `Bearer ${currentAccessToken}` };
        const response = await axios.post(urlPostOpportunity, profile, {
          headers,
          params: requestParams,
        });

        if (Math.floor(response.status / 100) === 2) {
          adapter.info('Successfully posted profile');
          i++;
        } else {
          failedProfiles.push(profile);
          adapter.error(`Failed to post profile. Status code: ${response.status}`);
          i++;
        }
      } catch (error: any) {
        if (error.response?.status === 429) {
          adapter.warn('Rate limit exceeded. Retrying after 1 minute.');
          await new Promise((resolve) => setTimeout(resolve, 60000));
        } else if (error.response?.status === 401) {
          adapter.warn('Access token has expired. Refreshing token and retrying.');
          const [newToken, newRefreshToken] = await getOrRefreshTokens(
            parameters.auth_domain,
            parameters.client_id,
            parameters.client_secret,
            'refresh_token',
            currentRefreshToken
          );
          currentAccessToken = newToken;
          currentRefreshToken = newRefreshToken;
        } else {
          failedProfiles.push(profile);
          adapter.error(`Failed to post profile. Error: ${error.message}`);
          i++;
        }
      }
    }

    return failedProfiles;
  } catch (error) {
    adapter.error(`Error in write: ${error}`);
    throw error;
  }
}

// Warehouse Definitions
export const LeverJobWarehouse = new Warehouse({
  name: 'Lever Jobs',
  dataSchema: LeverJob as any,
  dataType: DataType.job,
  read: new WarehouseReadAction({
    parameters: ReadJobsParameters,
    function: readJobs as any,
    endpoints: [GET_ALL_JOBS_ENDPOINT],
  }),
});

export const LeverProfileWarehouse = new Warehouse({
  name: 'Lever Profiles',
  dataSchema: LeverProfile as any,
  dataType: DataType.profile,
  read: new WarehouseReadAction({
    parameters: ReadProfilesParameters,
    function: readProfiles as any,
    endpoints: [GET_ALL_PROFILES_ENDPOINT],
  }),
  write: new WarehouseWriteAction({
    parameters: WriteProfileParameters,
    function: write as any,
    endpoints: [POST_PROFILE_ENDPOINT],
  }),
});
