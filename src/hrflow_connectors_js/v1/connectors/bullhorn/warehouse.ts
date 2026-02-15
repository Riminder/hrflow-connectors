/**
 * Bullhorn Warehouse
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
  ReadMode,
} from '../../core';
import {
  BullhornJob,
  BullhornProfile,
  BullhornAuthResponse,
  BullhornSearchResponse,
  BullhornEntityResponse,
  BullhornEntityFilesResponse,
  BullhornPagedResponse,
  ReadFromState,
  BullhornEntityFile,
} from './schemas';
import { auth } from './authentication';

/**
 * BaseParameters - Base parameters for Bullhorn authentication
 */
export class BaseParameters extends ParametersModel {
  client_id!: string;
  client_secret!: string;
  password!: string;
  username!: string;

  constructor(
    client_id: string,
    client_secret: string,
    password: string,
    username: string
  ) {
    super();
    this.client_id = client_id;
    this.client_secret = client_secret;
    this.password = password;
    this.username = username;
  }

  validate(): void {
    if (!this.client_id) throw new Error('client_id is required');
    if (!this.client_secret) throw new Error('client_secret is required');
    if (!this.password) throw new Error('password is required');
    if (!this.username) throw new Error('username is required');
  }
}

/**
 * WriteProfilesParameters - Parameters for profile write operations
 */
export class WriteProfilesParameters extends BaseParameters {
  constructor(
    client_id: string,
    client_secret: string,
    password: string,
    username: string
  ) {
    super(client_id, client_secret, password, username);
  }
}

/**
 * WriteApplicationsParameters - Parameters for application write operations
 */
export class WriteApplicationsParameters extends BaseParameters {
  job_id!: string;
  candidate_id?: string;
  status_when_created!: string;
  source?: string;

  constructor(
    client_id: string,
    client_secret: string,
    password: string,
    username: string,
    job_id: string,
    status_when_created: string,
    candidate_id?: string,
    source?: string
  ) {
    super(client_id, client_secret, password, username);
    this.job_id = job_id;
    this.candidate_id = candidate_id;
    this.status_when_created = status_when_created;
    this.source = source;
  }
}

/**
 * ReadParameters - Parameters for read operations with filtering
 */
export class ReadParameters extends BaseParameters {
  last_modified_date!: number;
  fields!: string;
  query!: string;
  count!: number;

  constructor(
    client_id: string,
    client_secret: string,
    password: string,
    username: string,
    last_modified_date: number,
    fields: string,
    query: string,
    count: number
  ) {
    super(client_id, client_secret, password, username);
    this.last_modified_date = last_modified_date;
    this.fields = fields;
    this.query = query;
    this.count = count;
  }
}

/**
 * ReadProfileParameters - Parameters for profile read operations
 */
export class ReadProfileParameters extends BaseParameters {
  constructor(
    client_id: string,
    client_secret: string,
    password: string,
    username: string
  ) {
    super(client_id, client_secret, password, username);
  }
}

/**
 * Authenticate helper - gets authentication info from Bullhorn
 */
async function authenticate(parameters: BaseParameters): Promise<BullhornAuthResponse> {
  return auth(
    parameters.username,
    parameters.password,
    parameters.client_id,
    parameters.client_secret
  );
}

/**
 * Make API request with token refresh on 401
 */
async function makeRequest(
  method: (url: string, config?: any) => Promise<AxiosResponse>,
  url: string,
  params: Record<string, any>,
  adapter: Logger,
  jsonData?: string
): Promise<AxiosResponse> {
  const config = jsonData ? { data: jsonData } : {};
  let response = await method(url, { params, ...config });

  if (response.status === 401) {
    adapter.info('Auth token expired, regenerating...');
    response = await method(url, { params, ...config });
  }

  return response;
}

/**
 * Handle API response
 */
function handleResponse(
  response: AxiosResponse,
  adapter: Logger
): Record<string, any> | null {
  if (!response.status || response.status < 200 || response.status >= 300) {
    adapter.error(
      `Request failed, status_code=${response.status}, response=${response.data}`
    );
    return null;
  }
  return response.data;
}

/**
 * Search for an entity in Bullhorn
 */
async function searchEntity(
  entity: string,
  restUrl: string,
  bhRestToken: string,
  query: string,
  fields: string,
  adapter: Logger
): Promise<BullhornSearchResponse | null> {
  const searchUrl = `${restUrl}search/${entity}`;
  const params = {
    BhRestToken: bhRestToken,
    query,
    fields,
    sort: 'id',
  };

  try {
    const response = await axios.get(searchUrl, { params });
    return handleResponse(response, adapter);
  } catch (error) {
    adapter.error(`Failed to search ${entity}: ${error}`);
    return null;
  }
}

/**
 * Create or update an entity in Bullhorn
 */
async function createOrUpdateEntity(
  entity: string,
  restUrl: string,
  params: Record<string, any>,
  data: Record<string, any>,
  adapter: Logger,
  entityId?: string
): Promise<BullhornEntityResponse | null> {
  const url = entityId
    ? `${restUrl}entity/${entity}/${entityId}`
    : `${restUrl}entity/${entity}`;
  const method = entityId ? axios.post : axios.put;

  try {
    const response = await method(url, data, { params });
    return handleResponse(response, adapter);
  } catch (error) {
    adapter.error(`Failed to create/update ${entity}: ${error}`);
    return null;
  }
}

/**
 * Check entity files
 */
async function checkEntityFiles(
  entity: string,
  restUrl: string,
  params: Record<string, any>,
  entityId: string,
  adapter: Logger
): Promise<BullhornEntityFilesResponse | null> {
  const url = `${restUrl}entityFiles/${entity}/${entityId}`;

  try {
    const response = await axios.get(url, { params });
    return handleResponse(response, adapter);
  } catch (error) {
    adapter.error(`Failed to check entity files: ${error}`);
    return null;
  }
}

/**
 * Write profiles to Bullhorn
 */
export async function write(
  adapter: Logger,
  parameters: WriteProfilesParameters,
  profiles: Record<string, any>[]
): Promise<Record<string, any>[]> {
  adapter.info(`Pushing ${profiles.length} profiles`);
  const failedProfiles: Record<string, any>[] = [];
  const authentication = await authenticate(parameters);

  for (const profile of profiles) {
    const profileBodyDict = profile;
    const createProfileBody = profileBodyDict.create_profile_body;
    const enrichProfileEducation = profileBodyDict.enrich_profile_education;
    const enrichProfileExperience = profileBodyDict.enrich_profile_experience;
    const enrichProfileAttachment = profileBodyDict.enrich_profile_attachment;

    const restUrl = authentication.restUrl;
    const params = { BhRestToken: authentication.BhRestToken };

    const candidateUrl = restUrl + 'entity/Candidate';

    try {
      const response = await axios.put(candidateUrl, createProfileBody, { params });

      if (!response.status || response.status < 200 || response.status >= 300) {
        adapter.error(
          `Failed to push profile to Bullhorn status_code=${response.status} response=${response.data}`
        );
        failedProfiles.push(profile);
        continue;
      }

      const candidateData = response.data;
      const candidateId = String(candidateData.changedEntityId);

      // Enrich profile education
      for (const education of enrichProfileEducation) {
        education.candidate = { id: candidateId };
        const educationUrl = restUrl + 'entity/CandidateEducation';
        await axios.put(educationUrl, education, { params });
      }

      // Enrich profile experience
      for (const experience of enrichProfileExperience) {
        experience.candidate = { id: candidateId };
        const experienceUrl = restUrl + 'entity/CandidateWorkHistory';
        await axios.put(experienceUrl, experience, { params });
      }

      // Enrich profile attachments
      for (const attachment of enrichProfileAttachment) {
        const attachmentUrl = `${restUrl}file/Candidate/${candidateId}`;
        await axios.put(attachmentUrl, attachment, { params });
      }
    } catch (error) {
      adapter.error(`Error processing profile: ${error}`);
      failedProfiles.push(profile);
    }
  }

  return failedProfiles;
}

/**
 * Write applications to Bullhorn
 */
export async function writeApplication(
  adapter: Logger,
  parameters: WriteApplicationsParameters,
  profiles: Record<string, any>[]
): Promise<Record<string, any>[]> {
  const failedProfiles: Record<string, any>[] = [];
  const authInfo = await authenticate(parameters);
  const restUrl = authInfo.restUrl;
  const bhRestToken = authInfo.BhRestToken;
  const params = { BhRestToken: bhRestToken };

  adapter.info(`connexion info ${JSON.stringify(params)}, rest_url: ${restUrl}`);

  for (const profile of profiles) {
    const comment = profile.comment;
    delete profile.comment;

    let candidateId = parameters.candidate_id;

    if (!parameters.candidate_id) {
      const attachment = profile.attachment;
      delete profile.attachment;

      profile.source = parameters.source || profile.source;
      profile.status = parameters.status_when_created || profile.status;
      const email = profile.email;

      adapter.info(`checking if candidate with ${email} already exists`);

      const searchResults = await searchEntity(
        'Candidate',
        restUrl,
        bhRestToken,
        `(email:${email} OR email2:${email}) AND isDeleted:0`,
        'id,isDeleted,dateAdded,status,source,email,firstName,lastName,name,mobile,address',
        adapter
      );

      if (!searchResults) {
        failedProfiles.push(profile);
        continue;
      }

      adapter.info(`search profile response ${JSON.stringify(searchResults)}`);

      const candidateExists = searchResults.count > 0;
      const candidateData = candidateExists ? searchResults.data[0] : {};
      candidateId = candidateExists ? candidateData.id : null;

      if (candidateExists) {
        Object.assign(profile, {
          firstName: candidateData.firstName || profile.firstName,
          lastName: candidateData.lastName || profile.lastName,
          name: candidateData.name || profile.name,
          address: candidateData.address || profile.address,
          mobile: candidateData.mobile || profile.mobile,
          status: candidateData.status || profile.status,
        });
      }

      adapter.info('creating or updating the candidate');
      const candidateResponse = await createOrUpdateEntity(
        'Candidate',
        restUrl,
        params,
        profile,
        adapter,
        candidateId
      );

      if (!candidateResponse) {
        failedProfiles.push(profile);
        continue;
      }

      adapter.info(`candidate creation response ${JSON.stringify(candidateResponse)}`);

      if (!candidateExists) {
        candidateId = candidateResponse.changedEntityId;
      }

      let attachmentExists = false;

      if (candidateExists && attachment) {
        const entityFiles = await checkEntityFiles(
          'Candidate',
          restUrl,
          params,
          candidateId,
          adapter
        );

        if (entityFiles) {
          const attachments = entityFiles.EntityFiles || [];
          if (attachments.length > 0 && attachment.name === attachments[0].name) {
            attachmentExists = true;
          }
        }
      }

      adapter.info(`attachment for the candidate exists ${attachmentExists}`);

      if (!attachmentExists && attachment) {
        const attachmentResponse = await makeRequest(
          (url: string, config?: any) =>
            axios.put(url, attachment, { params, ...config }),
          `${restUrl}file/Candidate/${candidateId}`,
          params,
          adapter,
          JSON.stringify(attachment)
        );

        const handled = handleResponse(attachmentResponse, adapter);
        if (!handled) {
          failedProfiles.push(profile);
          continue;
        }

        adapter.info(`attachment response ${JSON.stringify(handled)}`);
      }

      adapter.info(
        `Verifying if candidate had already applied for the job ${parameters.job_id}`
      );
    }

    const jobSubmissionResults = await searchEntity(
      'JobSubmission',
      restUrl,
      bhRestToken,
      `candidate.id:${candidateId} AND jobOrder.id:${parameters.job_id}`,
      'id,status,dateAdded',
      adapter
    );

    if (!jobSubmissionResults) {
      failedProfiles.push(profile);
      continue;
    }

    adapter.info(`search job_submission response ${JSON.stringify(jobSubmissionResults)}`);

    const jobSubmissionExists = jobSubmissionResults.count > 0;
    const jobSubmissionId = jobSubmissionExists
      ? jobSubmissionResults.data[0].id
      : null;

    const jobSubmissionPayload = {
      candidate: { id: candidateId },
      jobOrder: { id: parameters.job_id },
      status: parameters.status_when_created,
      source: parameters.source,
      dateWebResponse: Math.floor(Date.now()),
    };

    if (comment) {
      jobSubmissionPayload.comments = comment;
    }

    adapter.info('Creating or updating if candidate jobSubmission');

    const jobSubmissionResponse = await createOrUpdateEntity(
      'JobSubmission',
      restUrl,
      params,
      jobSubmissionPayload,
      adapter,
      jobSubmissionId
    );

    if (!jobSubmissionResponse) {
      failedProfiles.push(profile);
    }

    adapter.info(`creation of job_submission response ${JSON.stringify(jobSubmissionResponse)}`);
  }

  return failedProfiles;
}

/**
 * Read jobs from Bullhorn
 */
export async function* readJobs(
  adapter: Logger,
  parameters: ReadParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  let start = 0;
  let authRetries = 0;
  let totalReturned = 0;
  let shouldBreak = false;
  let authentication = await authenticate(parameters);

  let lastModifiedDate: number;
  let lastId: number = 0;

  if (readMode === ReadMode.sync) {
    if (parameters.last_modified_date === null) {
      throw new Error('last_modified_date cannot be null in ReadMode.sync');
    }
    lastModifiedDate = parameters.last_modified_date;
  } else {
    if (parameters.last_modified_date !== null) {
      adapter.warn('last_modified_date is ignored in ReadMode.incremental, using read_from instead');
    }

    if (readFrom) {
      try {
        const readFromObj = JSON.parse(readFrom) as ReadFromState;
        lastModifiedDate = readFromObj.last_modified_date;
        lastId = readFromObj.last_id;
      } catch (e) {
        throw new Error(`Failed to JSON parse read_from=${readFrom} error=${e}`);
      }
    } else {
      lastModifiedDate = parameters.last_modified_date;
    }
  }

  const lastModifiedDateFilter = transformTimestamp(lastModifiedDate);
  if (!lastModifiedDateFilter) {
    throw new Error(
      'error while applying a transformation date on last modified date to perform filtering'
    );
  }

  while (true) {
    try {
      const query =
        `${parameters.query} AND ` +
        `dateLastModified:[${lastModifiedDateFilter} TO *]`;

      const jobsUrl = `${authentication.restUrl}search/JobOrder`;
      const params: Record<string, any> = {
        query,
        fields: parameters.fields,
        sort: 'dateLastModified,id',
        start: String(start),
      };

      if (parameters.count) {
        params.count = parameters.count;
      }

      const headers = { BhRestToken: authentication.BhRestToken };

      const response = await axios.get(jobsUrl, { params, headers });
      const responseData = response.data;

      start = responseData.start + responseData.count;
      const data = responseData.data || [];

      for (const job of data) {
        if (parameters.count && totalReturned >= parameters.count) {
          shouldBreak = true;
          break;
        }

        if (
          readMode === ReadMode.incremental &&
          job.dateLastModified === lastModifiedDate &&
          job.id <= lastId
        ) {
          adapter.info('job with id <= last_id');
          continue;
        }

        yield job;
        totalReturned += 1;
      }

      if (shouldBreak) {
        break;
      }

      if (start >= responseData.total) {
        break;
      }
    } catch (e: any) {
      if (e.response?.status === 401) {
        adapter.info(
          'Received 401 error. Retrying authentication to continue fetching jobs.'
        );

        if (authRetries > 2) {
          throw new Error(
            `retries the authentication ${authRetries} will stop the execution`
          );
        }

        authentication = await authenticate(parameters);
        authRetries += 1;
        continue;
      } else {
        adapter.error('Failed to fetch jobs from Bullhorn.');
        throw e;
      }
    }
  }
}

/**
 * Read profiles with parsing (attachments, education, etc.)
 */
export async function* readProfilesParsing(
  adapter: Logger,
  parameters: ReadParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  let authentication = await authenticate(parameters);
  let start = 0;
  let authRetries = 0;
  let totalReturned = 0;
  let shouldBreak = false;
  let lastId: number = 0;

  let lastModifiedDate: number;

  if (readMode === ReadMode.sync) {
    if (parameters.last_modified_date === null) {
      throw new Error('last_modified_date cannot be null in ReadMode.sync');
    }
    lastModifiedDate = parameters.last_modified_date;
  } else {
    if (parameters.last_modified_date !== null) {
      adapter.warn(
        'last_modified_date is ignored in ReadMode.incremental, using read_from instead'
      );
    }

    if (readFrom) {
      try {
        const readFromObj = JSON.parse(readFrom) as ReadFromState;
        lastModifiedDate = readFromObj.last_modified_date;
        lastId = readFromObj.last_id;
      } catch (e) {
        throw new Error(`Failed to JSON parse read_from=${readFrom} error=${e}`);
      }
    } else {
      lastModifiedDate = parameters.last_modified_date;
    }
  }

  const lastModifiedDateFilter = transformTimestamp(lastModifiedDate);
  if (!lastModifiedDateFilter) {
    throw new Error(
      'Error while applying a transformation date on last modified date to perform filtering'
    );
  }

  while (true) {
    try {
      const query =
        `${parameters.query} AND ` +
        `dateLastModified:[${lastModifiedDateFilter} TO *]`;

      const profilesUrl = `${authentication.restUrl}search/Candidate`;
      const params: Record<string, any> = {
        query,
        fields: parameters.fields,
        sort: 'dateLastModified,id',
        start: String(start),
      };

      if (parameters.count) {
        params.count = parameters.count;
      }

      const headers = { BhRestToken: authentication.BhRestToken };

      const response = await axios.get(profilesUrl, { params, headers });

      if (!response.status || response.status < 200 || response.status >= 300) {
        adapter.error(
          `Failed to pull profiles from Bullhorn status_code=${response.status} response=${response.data}`
        );
        throw new Error('Failed to pull profiles from Bullhorn');
      }

      const responseData = response.data;
      start = responseData.start + responseData.count;
      const data = responseData.data || [];
      const total = responseData.total || 0;

      for (const profile of data) {
        if (parameters.count && totalReturned >= parameters.count) {
          shouldBreak = true;
          break;
        }

        if (
          readMode === ReadMode.incremental &&
          profile.dateLastModified === lastModifiedDate &&
          profile.id <= lastId
        ) {
          adapter.info('Profile with id <= last_id');
          continue;
        }

        profile.cvFile = null;

        const urlFiles = `${authentication.restUrl}entityFiles/Candidate/${profile.id}`;
        const fileHeaders = { BhRestToken: authentication.BhRestToken };

        try {
          const filesResponse = await axios.get(urlFiles, { headers: fileHeaders });
          const filesData = filesResponse.data;

          let lastCv: string | null = null;
          let currEntity: BullhornEntityFile | null = null;

          if (filesData.EntityFiles && filesData.EntityFiles.length > 0) {
            for (const entityFile of filesData.EntityFiles) {
              if (entityFile.type === 'Resume') {
                if (!currEntity) {
                  currEntity = entityFile;
                  lastCv = entityFile.id;
                } else if (currEntity.dateAdded < entityFile.dateAdded) {
                  currEntity = entityFile;
                  lastCv = entityFile.id;
                }
              }
            }
          }

          if (lastCv) {
            const urlCv = `${authentication.restUrl}/file/Candidate/${profile.id}/${lastCv}/raw`;
            const cvResponse = await axios.get(urlCv, {
              headers: fileHeaders,
              responseType: 'arraybuffer',
            });

            const file = cvResponse.data;
            profile.cvFile = file;
          }
        } catch (fileError) {
          adapter.warn(`Failed to fetch CV for profile ${profile.id}: ${fileError}`);
        }

        totalReturned += 1;
        yield profile;
      }

      if (shouldBreak) {
        break;
      }

      if (start >= total) {
        break;
      }
    } catch (e: any) {
      if (e.response?.status === 401) {
        adapter.info(
          'Received 401 error. Retrying authentication to continue fetching profiles.'
        );

        if (authRetries > 2) {
          throw new Error(
            `Retries exceeded for authentication (${authRetries}). Stopping execution.`
          );
        }

        authentication = await authenticate(parameters);
        authRetries += 1;
        continue;
      } else {
        adapter.error('Failed to fetch profiles from Bullhorn.');
        throw e;
      }
    }
  }
}

/**
 * Read profiles from Bullhorn
 */
export async function* readProfiles(
  adapter: Logger,
  parameters: ReadProfileParameters,
  readMode?: ReadMode,
  readFrom?: string
): AsyncIterable<Record<string, any>> {
  const authentication = await authenticate(parameters);

  const fields =
    'firstName,lastName,name,email,mobile,dateOfBirth,gender,address,dateAvailable,status,employeeType,activePlacements,skillSet,id,educations,workHistories';

  let start = 0;

  while (true) {
    const profilesUrl = `${authentication.restUrl}myCandidates?fields=${fields}&start=${start}`;
    const headers = { BhRestToken: authentication.BhRestToken };

    const response = await axios.get(profilesUrl, { headers });

    if (!response.status || response.status < 200 || response.status >= 300) {
      adapter.error(
        `Failed to pull profiles from Bullhorn status_code=${response.status} response=${response.data}`
      );
      throw new Error('Failed to pull profiles from Bullhorn');
    }

    const responseData = response.data;
    start = responseData.start + responseData.count;
    const total = responseData.total;
    const data = responseData.data || [];

    for (const profile of data) {
      // Enrich profile education
      const educationIds: string[] = [];
      const educations: Record<string, any>[] = [];

      if (profile.educations && profile.educations.data) {
        for (const ed of profile.educations.data) {
          educationIds.push(ed.id);
        }

        for (const id of educationIds) {
          const educationUrl =
            `${authentication.restUrl}entity/CandidateEducation/${id}?fields=*`;
          const edResponse = await axios.get(educationUrl, { headers });
          const edData = edResponse.data;
          educations.push(edData.data);
        }
      }

      // Enrich profile work history
      const workHistoryIds: string[] = [];
      const workHistories: Record<string, any>[] = [];

      if (profile.workHistories && profile.workHistories.data) {
        for (const workHistory of profile.workHistories.data) {
          workHistoryIds.push(workHistory.id);
        }

        for (const id of workHistoryIds) {
          const workHistoryUrl =
            `${authentication.restUrl}entity/CandidateWorkHistory/${id}?fields=*`;
          const whResponse = await axios.get(workHistoryUrl, { headers });
          const whData = whResponse.data;
          workHistories.push(whData.data);
        }
      }

      profile.educations = educations;
      profile.workHistories = workHistories;

      yield profile;
    }

    if (start >= total) {
      break;
    }
  }
}

/**
 * Transform timestamp from milliseconds to Bullhorn date format
 */
export function transformTimestamp(timestamp: number | null | undefined): string | null {
  if (timestamp === null || timestamp === undefined) {
    return null;
  }

  const date = new Date(Math.floor(timestamp));
  const year = date.getUTCFullYear();
  const month = String(date.getUTCMonth() + 1).padStart(2, '0');
  const day = String(date.getUTCDate()).padStart(2, '0');
  const hours = String(date.getUTCHours()).padStart(2, '0');
  const minutes = String(date.getUTCMinutes()).padStart(2, '0');
  const seconds = String(date.getUTCSeconds()).padStart(2, '0');

  return `${year}${month}${day}${hours}${minutes}${seconds}`;
}

/**
 * Convert item to read_from state
 */
export function itemToReadFrom(item: Record<string, any>): string {
  return JSON.stringify({
    last_modified_date: item.dateLastModified,
    last_id: item.id,
  });
}

/**
 * Bullhorn Profile Warehouse - Read/Write profiles
 */
export const BullhornProfileWarehouse = new Warehouse(
  'Bullhorn Profiles',
  BullhornProfile,
  DataType.profile,
  {
    write: {
      parameters: WriteProfilesParameters,
      function: write,
      endpoints: [],
    },
    read: {
      parameters: ReadProfileParameters,
      function: readProfiles,
      endpoints: [],
    },
  }
);

/**
 * Bullhorn Application Warehouse - Write applications
 */
export const BullhornApplicationWarehouse = new Warehouse(
  'Bullhorn Applications',
  BullhornProfile,
  DataType.profile,
  {
    write: {
      parameters: WriteApplicationsParameters,
      function: writeApplication,
      endpoints: [],
    },
  }
);

/**
 * Bullhorn Profile Parsing Warehouse - Read profiles with parsing
 */
export const BullhornProfileParsingWarehouse = new Warehouse(
  'Bullhorn Profiles',
  BullhornProfile,
  DataType.profile,
  {
    read: {
      parameters: ReadParameters,
      function: readProfilesParsing,
      endpoints: [],
      supportsIncremental: true,
      itemToReadFrom: itemToReadFrom,
    },
  }
);

/**
 * Bullhorn Job Warehouse - Read jobs
 */
export const BullhornJobWarehouse = new Warehouse(
  'Bullhorn Jobs',
  BullhornJob,
  DataType.job,
  {
    read: {
      parameters: ReadParameters,
      function: readJobs,
      supportsIncremental: true,
      itemToReadFrom: itemToReadFrom,
    },
  }
);
