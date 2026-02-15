/**
 * Workable Connector
 * Complete TypeScript translation of workable/connector.py
 */

import * as cheerio from 'cheerio';
import {
  ActionName,
  ActionType,
  BaseActionParameters,
  Connector,
  ConnectorAction,
  ConnectorType,
  WorkflowType,
} from '../../core';
import {
  WorkableJobWarehouse,
  WorkableProfileWarehouse,
} from './warehouse';
import { WorkableCandidate, WorkableJobModel } from './schemas';

// Placeholder types - would be imported from hrflow warehouse
interface HrFlowJobWarehouse {
  name: string;
}

interface HrFlowProfileWarehouse {
  name: string;
}

const HrFlowJobWarehouse: HrFlowJobWarehouse = { name: 'HrFlowJobWarehouse' };
const HrFlowProfileWarehouse: HrFlowProfileWarehouse = { name: 'HrFlowProfileWarehouse' };

// Helper function to remove HTML tags
function removeHtmlTags(text: string): string {
  if (!text) return '';
  return text.replace(/<[^<]+?>/g, '');
}

// Format a Workable job to HrFlow format
function formatJobs(workableJob: Record<string, any>): Record<string, any> {
  const hrflowJob: Record<string, any> = {};

  // Name and reference
  hrflowJob.name = workableJob.title;
  hrflowJob.reference = workableJob.shortcode;

  // URL
  hrflowJob.url = workableJob.url;

  // Location
  const location = workableJob.location || {};
  const locationStr = location.location_str;
  const geojson: Record<string, any> = {};

  if (location.country) geojson.country = location.country;
  if (location.country_code) geojson.country_code = location.country_code;
  if (location.region_code) geojson.region_code = location.region_code;
  if (location.region) geojson.region = location.region;
  if (location.city) geojson.city = location.city;
  if (location.zip_code) geojson.zip_code = location.zip_code;
  if (location.telecommuting !== undefined) geojson.telecommuting = location.telecommuting;

  hrflowJob.location = {
    text: typeof locationStr === 'string' ? locationStr : null,
    geojson,
  };

  // Sections
  hrflowJob.sections = [];

  const createSection = (fieldName: string, fieldDisplay: string): void => {
    const fieldValue = workableJob[fieldName];
    if (typeof fieldValue === 'string' && fieldValue.length > 0) {
      const description = removeHtmlTags(fieldValue);
      hrflowJob.sections.push({
        name: fieldDisplay,
        title: fieldDisplay,
        description,
      });
    }
  };

  createSection('description', 'workable_description');
  createSection('requirements', 'workable_requirements');
  createSection('benefit', 'workable_benefits');

  // Creation date
  hrflowJob.created_at = workableJob.created_at;

  // Tags
  hrflowJob.tags = [];

  const createTag = (fieldName: string): void => {
    const tagName = `workable_${fieldName}`;
    const fieldValue = workableJob[fieldName];
    if (fieldValue !== null && fieldValue !== undefined) {
      hrflowJob.tags.push({
        name: tagName,
        value: String(fieldValue),
      });
    }
  };

  createTag('employment_type');
  createTag('full_title');
  createTag('id');
  createTag('code');
  createTag('state');
  createTag('department');
  createTag('application_url');
  createTag('shortlink');

  return hrflowJob;
}

// Format an HrFlow profile to Workable candidate format
function formatProfile(hrflowProfile: Record<string, any>): string {
  const candidateProfile: Record<string, any> = {};
  const info = hrflowProfile.info || {};

  candidateProfile.name = info.full_name;
  candidateProfile.summary = info.summary;
  candidateProfile.email = info.email;
  candidateProfile.phone = info.phone;

  const location = info.location || {};
  if (typeof location.text === 'string') {
    candidateProfile.address = location.text;
  }

  const attachments = hrflowProfile.attachments || [];
  if (Array.isArray(attachments)) {
    for (const attachment of attachments) {
      if (typeof attachment === 'object' && attachment.type === 'resume') {
        candidateProfile.resume_url = attachment.public_url;
        break;
      }
    }
  }

  const workableProfile = {
    sourced: true,
    candidate: candidateProfile,
  };

  return JSON.stringify(workableProfile);
}

// Connector definition
const DESCRIPTION =
  'Workable is an applicant tracking system that helps teams find candidates, evaluate applicants and make the right hire, faster.';

export const Workable = new Connector({
  name: 'Workable',
  type: ConnectorType.ATS,
  subtype: 'workable',
  description: DESCRIPTION,
  url: 'https://www.workable.com/',
  actions: [
    new ConnectorAction({
      name: ActionName.pull_job_list,
      triggerType: WorkflowType.pull,
      description:
        'Retrieves all jobs via the Workable API and sends them to a Hrflow.ai Board.',
      parameters: BaseActionParameters.withDefaults(
        'PullJobsActionParameters',
        formatJobs
      ),
      origin: WorkableJobWarehouse,
      target: HrFlowJobWarehouse as any,
      actionType: ActionType.inbound,
    }),
    new ConnectorAction({
      name: ActionName.push_profile,
      triggerType: WorkflowType.catch,
      description:
        'Writes a profile from Hrflow.ai Source to Workable via the API.',
      parameters: BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatProfile
      ),
      origin: HrFlowProfileWarehouse as any,
      target: WorkableProfileWarehouse,
      actionType: ActionType.outbound,
    }),
  ],
});

export default Workable;
