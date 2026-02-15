/**
 * Bullhorn Schemas
 * Complete TypeScript translation of schemas.py
 */

/**
 * BullhornAddress - Candidate address information
 */
export interface BullhornAddress {
  address1?: string;
  city?: string;
  state?: string;
  zip?: string;
}

/**
 * BullhornCandidate - Candidate reference for relationships
 */
export interface BullhornCandidate {
  id?: number;
}

/**
 * BullhornProfile - Main candidate/profile data
 */
export interface BullhornProfile {
  id?: string;
  address?: BullhornAddress;
  certifications?: any;
  name?: string;
  firstName?: string;
  lastName?: string;
  email?: string;
  mobile?: string;
  dateOfBirth?: number;
  experience?: number;
  skillSet?: string;
  dateAvailable?: number;
  status?: string;
  employeeType?: string;
  activePlacements?: { total: number };
  educations?: BullhornEducationEnrichment[];
  workHistories?: BullhornExperienceEnrichment[];
  gender?: string;
  cvFile?: Buffer;
  dateLastModified?: number;
}

/**
 * BullhornAttachmentEnrichment - File attachment data
 */
export interface BullhornAttachmentEnrichment {
  externalID?: string;
  fileContent?: string;
  fileExtension?: string;
  fileType?: string;
  name?: string;
  contentType?: string;
  description?: string;
  type?: string;
  format?: string;
}

/**
 * BullhornExperienceEnrichment - Work history/experience data
 */
export interface BullhornExperienceEnrichment {
  id?: string;
  candidate: BullhornCandidate;
  companyName?: string;
  title?: string;
  comments?: string;
  startDate?: number;
  endDate?: number;
}

/**
 * BullhornEducationEnrichment - Education data
 */
export interface BullhornEducationEnrichment {
  id?: string;
  candidate: BullhornCandidate;
  school?: string;
  degree?: string;
  comments?: string;
  city?: string;
  startDate?: number;
  endDate?: number;
  certification?: string;
}

/**
 * BullhornJob - Job posting data
 */
export interface BullhornJob {
  id?: number;
  title?: string;
  publicDescription?: string;
  address?: BullhornAddress;
  skillList?: string;
  degreeList?: string[];
  durationWeeks?: number;
  employmentType?: string;
  numOpenings?: number;
  onSite?: string;
  salaryUnit?: string;
  startDate?: number;
  status?: string;
  type?: string;
  willRelocate?: string;
  salary?: number;
  isWorkFromHome?: string;
  hoursPerWeek?: number;
  hoursOfOperation?: string;
  dateAdded?: number;
  dateLastModified?: number;
}

/**
 * BullhornJobSubmission - Job submission/application data
 */
export interface BullhornJobSubmission {
  id?: string;
  candidate: BullhornCandidate;
  jobOrder?: { id: string };
  status?: string;
  source?: string;
  dateWebResponse?: number;
  comments?: string;
  dateAdded?: number;
}

/**
 * BullhornEntityFile - File metadata from entity files
 */
export interface BullhornEntityFile {
  id: string;
  name: string;
  type: string;
  dateAdded: number;
}

/**
 * BullhornEntityFilesResponse - Response from entity files endpoint
 */
export interface BullhornEntityFilesResponse {
  EntityFiles: BullhornEntityFile[];
}

/**
 * BullhornAuthResponse - OAuth authentication response
 */
export interface BullhornAuthResponse {
  restUrl: string;
  BhRestToken: string;
  userId: string;
  refresh_token?: string;
}

/**
 * BullhornSearchResponse - Search response from Bullhorn API
 */
export interface BullhornSearchResponse {
  data: Record<string, any>[];
  count: number;
  total: number;
  start: number;
}

/**
 * BullhornPagedResponse - Generic paged response
 */
export interface BullhornPagedResponse {
  data: Record<string, any>[];
  count: number;
  total: number;
  start: number;
}

/**
 * BullhornEntityResponse - Response when creating/updating entity
 */
export interface BullhornEntityResponse {
  changedEntityId?: string;
  changeType?: string;
}

/**
 * ReadFromState - State object for incremental reads
 */
export interface ReadFromState {
  last_modified_date: number;
  last_id: number;
}

/**
 * HrFlowProfile - HrFlow profile format
 */
export interface HrFlowProfile {
  reference?: string;
  info?: {
    full_name?: string;
    first_name?: string;
    last_name?: string;
    email?: string;
    phone?: string;
    date_birth?: string;
    location?: {
      text?: string;
      fields?: Record<string, any>;
    };
    gender?: string;
  };
  skills?: Array<{ name: string }>;
  experiences?: Array<{
    company?: string;
    title?: string;
    description?: string;
    date_start?: string;
    date_end?: string;
  }>;
  educations?: Array<{
    school?: string;
    title?: string;
    description?: string;
    date_start?: string;
    date_end?: string;
    location?: { text?: string };
  }>;
  attachments?: Array<{
    public_url: string;
    file_name: string;
  }>;
  experiences_duration?: number;
  metadatas?: Array<{ name: string; value: any }>;
  tags?: Array<{ name: string; value: any }>;
}

