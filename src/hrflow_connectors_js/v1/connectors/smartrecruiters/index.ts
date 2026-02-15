/**
 * SmartRecruiters Connector
 * Complete TypeScript translation of smartrecruiters/connector.py
 */

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
  SmartRecruitersJobWarehouse,
  SmartRecruitersProfileWarehouse,
} from './warehouse';

// Placeholder types
interface HrFlowJobWarehouse {
  name: string;
}

interface HrFlowProfileWarehouse {
  name: string;
}

const HrFlowJobWarehouse: HrFlowJobWarehouse = { name: 'HrFlowJobWarehouse' };
const HrFlowProfileWarehouse: HrFlowProfileWarehouse = { name: 'HrFlowProfileWarehouse' };

// Helper function to get job location
function getJobLocation(smartrecruitersLocation: Record<string, any> | null): Record<string, any> {
  if (!smartrecruitersLocation) {
    return { lat: null, lng: null, text: '' };
  }

  let lat: number | null = smartrecruitersLocation.latitude;
  if (lat !== null && lat !== undefined) {
    lat = parseFloat(String(lat));
  }

  let lng: number | null = smartrecruitersLocation.longitude;
  if (lng !== null && lng !== undefined) {
    lng = parseFloat(String(lng));
  }

  const concatenate: string[] = [];
  for (const field of ['country', 'region', 'city', 'address']) {
    const value = smartrecruitersLocation[field];
    if (value) {
      concatenate.push(value);
    }
  }

  return {
    lat,
    lng,
    text: concatenate.join(' '),
  };
}

// Helper function to get job sections
function getSections(smartrecruitersJob: Record<string, any>): Array<Record<string, any>> {
  const sections: Array<Record<string, any>> = [];

  if (
    !smartrecruitersJob.jobAd ||
    !smartrecruitersJob.jobAd.sections
  ) {
    return sections;
  }

  const smartrecruitersections = smartrecruitersJob.jobAd.sections;
  const sectionNames = [
    'companyDescription',
    'jobDescription',
    'qualifications',
    'additionalInformation',
  ];

  for (const sectionName of sectionNames) {
    const section = smartrecruitersections[sectionName];
    if (section) {
      sections.push({
        name: `smartrecruiters_jobAd-sections-${sectionName}`,
        title: section.title,
        description: section.text,
      });
    }
  }

  return sections;
}

// Helper function to get job tags
function getTags(smartrecruitersJob: Record<string, any>): Array<Record<string, any>> {
  const job = smartrecruitersJob;
  const creator = job.creator || {};
  const compensation = job.compensation || {};

  const createTag = (name: string, value: any): Record<string, any> => ({
    name,
    value,
  });

  return [
    createTag('smartrecruiters_status', job.status),
    createTag('smartrecruiters_postingStatus', job.postingStatus),
    createTag('smartrecruiters_id', job.id),
    createTag('smartrecruiters_experienceLevel-id', job.experienceLevel?.id),
    createTag('smartrecruiters_typeOfEmployment-id', job.typeOfEmployment?.id),
    createTag('smartrecruiters_compensation-min', compensation.min),
    createTag('smartrecruiters_compensation-max', compensation.max),
    createTag('smartrecruiters_compensation-currency', compensation.currency),
    createTag('smartrecruiters_industry-id', job.industry?.id),
    createTag('smartrecruiters_creator-firstName', creator.firstName),
    createTag('smartrecruiters_creator-lastName', creator.lastName),
    createTag('smartrecruiters_function-id', job.function?.id),
    createTag('smartrecruiters_department-id', job.department?.id),
    createTag('smartrecruiters_location-manual', job.location?.manual),
    createTag('smartrecruiters_location-remote', job.location?.remote),
    createTag('smartrecruiters_eeoCategory-id', job.eeoCategory?.id),
    createTag('smartrecruiters_targetHiringDate', job.targetHiringDate),
  ].filter((tag) => tag.value !== null && tag.value !== undefined);
}

// Format a SmartRecruiters job to HrFlow format
function formatJob(smartrecruitersJob: Record<string, any>): Record<string, any> {
  return {
    name: smartrecruitersJob.title || 'Undefined',
    reference: smartrecruitersJob.refNumber,
    created_at: smartrecruitersJob.createdOn,
    updated_at: smartrecruitersJob.updatedOn,
    location: getJobLocation(smartrecruitersJob.location),
    url: null,
    summary: null,
    sections: getSections(smartrecruitersJob),
    tags: getTags(smartrecruitersJob),
  };
}

// Helper function to get profile location
function getProfileLocation(hrflowLocation: Record<string, any>): Record<string, any> {
  const fields = hrflowLocation.fields || {};
  return {
    lat: hrflowLocation.lat || 0,
    lng: hrflowLocation.lng || 0,
    city: fields.city || 'Undefined',
    country: fields.country || 'Undefined',
    region: fields.state || 'Undefined',
  };
}

// Helper function to get profile occupation (for education/experience)
function getProfileOccupation(hrflowOccupation: Record<string, any>): Record<string, any> {
  const dateStart = (hrflowOccupation.date_start || 'XXXX').split('T')[0];
  const dateEnd = (hrflowOccupation.date_end || 'XXXX').split('T')[0];
  const location = hrflowOccupation.location?.text || 'Undefined';

  return {
    description: hrflowOccupation.description,
    current: false,
    startDate: dateStart,
    endDate: dateEnd,
    location,
  };
}

// Helper function to get profile experiences
function getProfileExperiences(hrflowExperiences: Array<Record<string, any>>): Array<Record<string, any>> {
  return hrflowExperiences.map((experience) => ({
    title: experience.title || 'Undefined',
    company: experience.company || 'Undefined',
    ...getProfileOccupation(experience),
  }));
}

// Helper function to get profile educations
function getProfileEducations(hrflowEducations: Array<Record<string, any>>): Array<Record<string, any>> {
  return hrflowEducations.map((education) => ({
    institution: education.school || 'Undefined',
    degree: education.title || 'Undefined',
    major: 'Undefined',
    ...getProfileOccupation(education),
  }));
}

// Format an HrFlow profile to SmartRecruiters candidate format
function formatProfile(hrflowProfile: Record<string, any>): Record<string, any> {
  const hrflowProfileInfo = hrflowProfile.info;

  return {
    firstName: hrflowProfileInfo.first_name,
    lastName: hrflowProfileInfo.last_name,
    email: hrflowProfileInfo.email,
    phoneNumber: hrflowProfileInfo.phone,
    location: getProfileLocation(hrflowProfileInfo.location),
    experiences: getProfileExperiences(hrflowProfile.experiences || []),
    educations: getProfileEducations(hrflowProfile.educations || []),
    web: hrflowProfileInfo.urls || {},
    tags: [],
    consent: true,
    attachments: hrflowProfile.attachments || [],
  };
}

// Connector definition
const DESCRIPTION =
  'Move beyond applicant tracking systems (ATS) with an enterprise-grade recruiting platform designed for the modern workforce. SmartRecruiters\' Talent Acquisition Suite provides everything needed to attract, select, and hire great talent.';

export const SmartRecruiters = new Connector({
  name: 'SmartRecruiters',
  type: ConnectorType.ATS,
  subtype: 'smartrecruiters',
  description: DESCRIPTION,
  url: 'https://www.smartrecruiters.com/',
  actions: [
    new ConnectorAction({
      name: ActionName.pull_job_list,
      triggerType: WorkflowType.pull,
      description:
        'Retrieves all jobs via the SmartRecruiter API and send them to a Hrflow.ai Board.',
      parameters: BaseActionParameters.withDefaults(
        'ReadJobsActionParameters',
        formatJob
      ),
      origin: SmartRecruitersJobWarehouse,
      target: HrFlowJobWarehouse as any,
      actionType: ActionType.inbound,
    }),
    new ConnectorAction({
      name: ActionName.push_profile,
      triggerType: WorkflowType.catch,
      description:
        'Writes a profile from Hrflow.ai Source to SmartRecruiters via the API for the given `job_id`.',
      parameters: BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatProfile
      ),
      origin: HrFlowProfileWarehouse as any,
      target: SmartRecruitersProfileWarehouse,
      actionType: ActionType.outbound,
    }),
  ],
});

export default SmartRecruiters;
