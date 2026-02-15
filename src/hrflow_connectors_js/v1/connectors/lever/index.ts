/**
 * Lever Connector
 * Complete TypeScript translation of lever/connector.py
 */

import { DateTime } from 'luxon';
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
  LeverJobWarehouse,
  LeverProfileWarehouse,
} from './warehouse';

// Placeholder types - would be imported from hrflow warehouse
interface HrFlowJobWarehouse {
  name: string;
}

interface HrFlowProfileWarehouse {
  name: string;
}

const HrFlowJobWarehouse: HrFlowJobWarehouse = { name: 'HrFlowJobWarehouse' };
const HrFlowProfileWarehouse: HrFlowProfileWarehouse = { name: 'HrFlowProfileWarehouse' };

// Helper function to extract job sections from HTML content
function extractJobSections(contents: Record<string, any>): Array<Record<string, any>> {
  const lists = contents.lists || [];
  const sections: Array<Record<string, any>> = [];

  for (const item of lists) {
    const content = item.content;
    const contentText = htmlToPlainText(content);
    const section = {
      name: item.text || null,
      title: item.text || null,
      description: contentText,
    };
    if (section) {
      sections.push(section);
    }
  }

  return sections;
}

// Helper function to convert HTML to plain text
function htmlToPlainText(html: string): string {
  if (!html) return '';
  const $ = cheerio.load(html);
  const plainText = $.text();
  return plainText;
}

// Extract tags from Lever job
function getJobTags(leverJob: Record<string, any>): Array<Record<string, string>> {
  const tags: Array<Record<string, string>> = [];

  tags.push({
    name: 'lever_confidential',
    value: leverJob.confidentiality === 'non-confidential' ? 'False' : 'True',
  });

  const categories = leverJob.categories || {};
  for (const [tagName, tagValue] of Object.entries(categories)) {
    tags.push({
      name: `lever_${tagName}`,
      value: String(tagValue),
    });
  }

  const additionalTags = leverJob.tags || [];
  for (let i = 0; i < additionalTags.length; i++) {
    tags.push({
      name: `lever_additional_tag_${i}`,
      value: additionalTags[i],
    });
  }

  const distributionChannels = leverJob.distributionChannels || [];
  for (let index = 0; index < distributionChannels.length; index++) {
    tags.push({
      name: `lever_distributionChannel_${index}`,
      value: distributionChannels[index],
    });
  }

  const urls = leverJob.urls || {};
  for (const [item, value] of Object.entries(urls)) {
    tags.push({
      name: `lever_url_${item}`,
      value: String(value),
    });
  }

  if (leverJob.country) {
    tags.push({
      name: 'lever_country',
      value: leverJob.country,
    });
  }

  if (leverJob.state) {
    tags.push({
      name: 'lever_state',
      value: leverJob.state,
    });
  }

  if (leverJob.workplaceType) {
    tags.push({
      name: 'lever_workplaceType',
      value: leverJob.workplaceType,
    });
  }

  return tags;
}

// Extract salary ranges
function getJobRangesFloat(leverJob: Record<string, any>): Array<Record<string, any>> {
  const rangesList: Array<Record<string, any>> = [];
  const salaryRange = leverJob.salaryRange || {};

  if (salaryRange && Object.keys(salaryRange).length > 0) {
    rangesList.push({
      name: 'salary',
      value_min: salaryRange.min || null,
      value_max: salaryRange.max || null,
      unit: salaryRange.currency || null,
    });
  }

  return rangesList;
}

// Format a Lever job to HrFlow format
function formatJob(leverJobData: Record<string, any>): Record<string, any> {
  const createdAt = new Date(leverJobData.createdAt).toISOString();
  const updatedAt = new Date(leverJobData.updatedAt).toISOString();

  const job: Record<string, any> = {
    reference: leverJobData.id,
    name: leverJobData.text,
    location: {
      text: leverJobData.categories?.location || null,
      lat: null,
      lng: null,
    },
    url: leverJobData.urls?.show,
    summary: leverJobData.content?.description,
    sections: extractJobSections(leverJobData.content),
    tags: getJobTags(leverJobData),
    ranges_float: getJobRangesFloat(leverJobData),
    created_at: createdAt,
    updated_at: updatedAt,
  };

  return job;
}

// Get profile experiences from parsed resume data
function getProfileExperiences(leverProfileData: Record<string, any>): Array<Record<string, any>> {
  if (!leverProfileData) return [];

  const experiencesData = leverProfileData.parsedData?.positions || [];
  const experiences: Array<Record<string, any>> = [];

  for (const experienceData of experiencesData) {
    const startData = experienceData.start || {};
    const endData = experienceData.end || {};
    const startYear = startData.year;
    const startMonth = startData.month;
    const endYear = endData.year;
    const endMonth = endData.month;

    let startDate: string | null = null;
    if (startYear && startMonth) {
      startDate = new Date(startYear, startMonth - 1, 1).toISOString();
    }

    let endDate: string | null = null;
    if (endYear && endMonth) {
      endDate = new Date(endYear, endMonth - 1, 1).toISOString();
    }

    const experience: Record<string, any> = {
      company: experienceData.org,
      title: experienceData.title,
      description: experienceData.summary,
      location: {
        text: experienceData.location,
        lat: null,
        lng: null,
      },
      date_start: startDate,
      date_end: endDate,
      skills: [],
    };

    experiences.push(experience);
  }

  return experiences;
}

// Get profile educations from parsed resume data
function getProfileEducations(leverProfileData: Record<string, any>): Array<Record<string, any>> {
  if (!leverProfileData) return [];

  const educationsData = leverProfileData.parsedData?.schools || [];
  const educations: Array<Record<string, any>> = [];

  for (const educationData of educationsData) {
    const startData = educationData.start || {};
    const endData = educationData.end || {};
    const startYear = startData.year;
    const startMonth = startData.month;
    const endYear = endData.year;
    const endMonth = endData.month;

    let startDate: string | null = null;
    if (startYear && startMonth) {
      startDate = new Date(startYear, startMonth - 1, 1).toISOString();
    }

    let endDate: string | null = null;
    if (endYear && endMonth) {
      endDate = new Date(endYear, endMonth - 1, 1).toISOString();
    }

    const education: Record<string, any> = {
      school: educationData.org,
      title: educationData.degree,
      description: educationData.summary,
      location: {
        text: educationData.location,
        lat: null,
        lng: null,
      },
      date_start: startDate,
      date_end: endDate,
      skills: [],
    };

    educations.push(education);
  }

  return educations;
}

// Get attachments from profile data
function getProfileAttachments(leverProfileData: Record<string, any>): Array<Record<string, any>> {
  const attachments: Array<Record<string, any>> = [];

  if (!leverProfileData) return attachments;

  const attachmentsData = leverProfileData.file;

  if (attachmentsData) {
    const attachmentDict: Record<string, any> = {
      type: 'original',
      file_name: attachmentsData.name,
      original_file_name: attachmentsData.name,
      extension: attachmentsData.ext,
      public_url: attachmentsData.downloadUrl,
      file_size: attachmentsData.size,
      created_at: new Date(attachmentsData.uploadedAt).toISOString(),
    };
    attachments.push(attachmentDict);
  }

  return attachments;
}

// Format profile URLs
function formatProfileUrls(leverLinks: any[]): Array<Record<string, string>> {
  const urls: Array<Record<string, string>> = [];

  if (!leverLinks) return urls;

  for (const link of leverLinks) {
    urls.push({
      type: 'from lever',
      url: link,
    });
  }

  return urls;
}

// Extract first and last name from full name
function extractFirstLastName(fullName: string): [string, string | null] {
  const names = fullName.split(' ');
  const firstName = names[0];
  const lastName = names.length > 1 ? names.slice(1).join(' ') : null;
  return [firstName, lastName];
}

// Format a Lever opportunity (profile) to HrFlow format
function formatProfile(opportunityData: Record<string, any>): Record<string, any> {
  const leverOpportunity = opportunityData;
  const leverProfile = leverOpportunity.profile?.[0] || {};

  const createdAt = new Date(leverOpportunity.createdAt).toISOString();
  const updatedAt = new Date(leverOpportunity.updatedAt).toISOString();

  const [firstName, lastName] = extractFirstLastName(leverOpportunity.name);

  const profile: Record<string, any> = {
    reference: leverOpportunity.id,
    updated_at: updatedAt,
    created_at: createdAt,
    info: {
      full_name: leverOpportunity.name,
      first_name: firstName,
      last_name: lastName,
      email: leverOpportunity.emails?.[0] || null,
      phone: leverOpportunity.phones?.[0]?.value || null,
      location: leverOpportunity.location
        ? { text: leverOpportunity.location, lat: null, lng: null }
        : { text: null, lat: null, lng: null },
      urls: formatProfileUrls(leverOpportunity.links),
    },
    text_language: null,
    text: leverOpportunity.headline || null,
    experiences: getProfileExperiences(leverProfile),
    educations: getProfileEducations(leverProfile),
    attachments: getProfileAttachments(leverProfile),
    skills: [],
    tags: [],
  };

  return profile;
}

// Get URLs from profile
function getProfileUrls(urls: Array<Record<string, string>>): string[] {
  const urlsList: string[] = [];

  if (!urls) return urlsList;

  for (const urlDict of urls) {
    urlsList.push(urlDict.url);
  }

  return urlsList;
}

// Get skills from profile
function getProfileSkills(skills: Array<Record<string, string>>): string[] {
  const leverTags: string[] = [];

  if (!skills) return leverTags;

  for (const skillDict of skills) {
    leverTags.push(skillDict.name);
  }

  return leverTags;
}

// Convert ISO date to timestamp (milliseconds)
function fromIsoToTimestamp(date: string): number | null {
  if (!date) return null;

  try {
    const timestamp = new Date(date).getTime();
    return timestamp;
  } catch (error) {
    return null;
  }
}

// Construct headline from profile experiences and educations
function constructHeadline(hrflowProfile: Record<string, any>): string | null {
  const experiences = hrflowProfile.experiences || [];
  const educations = hrflowProfile.educations || [];
  const listOfEntityTitle: string[] = [];

  for (const experience of experiences) {
    if (experience.company) {
      listOfEntityTitle.push(experience.company);
    }
  }

  for (const education of educations) {
    if (education.school) {
      listOfEntityTitle.push(education.school);
    }
  }

  if (listOfEntityTitle.length === 0) return null;

  return listOfEntityTitle.join(' , ');
}

// Format HrFlow profile to Lever opportunity format
function formatOpportunity(hrflowProfile: Record<string, any>): Record<string, any> {
  const leverOpportunity: Record<string, any> = {
    name: hrflowProfile.info?.full_name,
    headline: constructHeadline(hrflowProfile),
    location: hrflowProfile.info?.location?.text,
    phones: hrflowProfile.info?.phone
      ? [{ type: 'mobile', value: hrflowProfile.info.phone }]
      : [],
    emails: hrflowProfile.info?.email ? [hrflowProfile.info.email] : [],
    links: getProfileUrls(hrflowProfile.info?.urls || []),
    tags: getProfileSkills(hrflowProfile.skills || []),
    createdAt: fromIsoToTimestamp(hrflowProfile.created_at),
  };

  if (hrflowProfile.attachments?.[0]) {
    leverOpportunity.file = hrflowProfile.attachments[0];
  }

  return leverOpportunity;
}

// Connector definition
const DESCRIPTION =
  'Lever is a modern recruitment platform that helps companies streamline their hiring process.';

export const Lever = new Connector({
  name: 'Lever',
  type: ConnectorType.ATS,
  subtype: 'lever',
  description: DESCRIPTION,
  url: 'https://www.lever.co/',
  actions: [
    new ConnectorAction({
      name: ActionName.pull_job_list,
      triggerType: WorkflowType.pull,
      description:
        'Retrieves all jobs via the Lever API and sends them to the Hrflow.ai Board.',
      parameters: BaseActionParameters.withDefaults('ReadJobsActionParameters', formatJob),
      origin: LeverJobWarehouse,
      target: HrFlowJobWarehouse as any,
      actionType: ActionType.inbound,
    }),
    new ConnectorAction({
      name: ActionName.push_profile,
      triggerType: WorkflowType.catch,
      description: 'Writes a profile from the Hrflow.ai Source to Lever via the API.',
      parameters: BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatOpportunity
      ),
      origin: HrFlowProfileWarehouse as any,
      target: LeverProfileWarehouse,
      actionType: ActionType.outbound,
    }),
    new ConnectorAction({
      name: ActionName.pull_profile_list,
      triggerType: WorkflowType.pull,
      description: 'Read a profile from Lever Source to Hrflow.ai via the API.',
      parameters: BaseActionParameters.withDefaults(
        'ReadProfilesParameters',
        formatProfile
      ),
      origin: LeverProfileWarehouse,
      target: HrFlowProfileWarehouse as any,
      actionType: ActionType.inbound,
    }),
  ],
});

export default Lever;
