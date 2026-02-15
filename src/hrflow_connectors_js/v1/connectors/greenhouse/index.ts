/**
 * Greenhouse Connector
 * Complete TypeScript translation of greenhouse/connector.py
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
  GreenhouseJobWarehouse,
  GreenhouseProfileWarehouse,
} from './warehouse';

// These would need to be imported from an HRFlow warehouse
// For now, we define placeholder types
interface HrFlowJobWarehouse {
  name: string;
}

interface HrFlowProfileWarehouse {
  name: string;
}

const HrFlowJobWarehouse: HrFlowJobWarehouse = { name: 'HrFlowJobWarehouse' };
const HrFlowProfileWarehouse: HrFlowProfileWarehouse = { name: 'HrFlowProfileWarehouse' };

// Helper function - would be imported from hrflow warehouse
function removeHtmlTags(html: string): string {
  if (!html) return '';
  return html.replace(/<[^>]*>/g, '');
}

// Constants
const APPLICATION_TAG = 'application_boardKey_jobReference';

// Format functions
export function formatJob(data: Record<string, any>): Record<string, any> {
  /**
   * Format each job pulled from greenhouse job board into a HrFlow job object
   * Returns:
   *   HrflowJob: job in the HrFlow job object format
   */
  const job: Record<string, any> = {};

  // name
  job.name = data.title;

  // summary
  job.summary = null;

  // reference
  job.reference = String(data.id);

  // url
  job.url = data.absolute_url;

  // location
  const location = data.location?.name || '';
  job.location = {
    text: location,
    lat: null,
    lng: null,
  };

  // sections
  const descriptionContent = data.content || '';
  const text = removeHtmlTags(descriptionContent);

  job.sections = [
    {
      name: 'greenhouse_description',
      title: 'greenhouse_description',
      description: text,
    },
  ];

  // metadata
  job.metadatas = data.metadata;

  // tags
  const department = data.departments || [];
  let departmentName = 'Undefined';
  let departmentId = 'Undefined';

  if (department && department.length > 0) {
    departmentName = department[0].name;
    departmentId = String(department[0].id);
  }

  const office = data.offices || [];
  let officeName = 'Undefined';
  let officeId = 'Undefined';

  if (office && office.length > 0) {
    officeName = office[0].name;
    officeId = String(office[0].id);
  }

  const education = data.education || null;
  const employment = data.employment || null;

  job.tags = [
    { name: 'greenhouse_department-name', value: departmentName },
    { name: 'greenhouse_department-id', value: departmentId },
    { name: 'greenhouse_office-location', value: officeName },
    { name: 'greenhouse_office-id', value: officeId },
    { name: 'greenhouse_education', value: education },
    { name: 'greenhouse_employment', value: employment },
  ];

  // updated_at
  job.updated_at = data.updated_at;

  return job;
}

export function formatProfile(data: Record<string, any>): Record<string, any> {
  /**
   * Format a profile hrflow object to a greenhouse profile object
   * Args:
   *   profile (HrflowProfile): profile object in the hrflow profile format
   * Returns:
   *   GreenhouseProfileModel: profile in the greenhouse candidate format
   */
  const profile: Record<string, any> = {};
  profile.applications = [];

  // Extract job IDs from tags
  const tags = data.tags || [];
  const applications = tags.filter((x: any) => x.name === APPLICATION_TAG);
  const jobIdList = applications.map((x: any) => {
    const parts = x.value.split('_');
    return parseInt(parts[1], 10);
  });

  if (jobIdList.length === 0) {
    throw new Error(
      `No job_id found, tag named '${APPLICATION_TAG}' either non existent or name poorly formatted.`
    );
  }

  for (const id of jobIdList) {
    profile.applications.push({ job_id: id });
  }

  const info = data.info || {};
  profile.first_name = info.first_name;
  profile.last_name = info.last_name;
  profile.external_id = data.reference;

  // Resume
  const attachments = data.attachments || [];
  if (attachments.length > 0) {
    profile.resume = attachments[0].public_url;
  }

  // Contact info
  const phone = info.phone || null;
  if (phone) {
    profile.phone_numbers = [{ value: phone, type: 'mobile' }];
  }

  const email = info.email || null;
  if (email) {
    profile.email_addresses = [{ value: email, type: 'personal' }];
  }

  const location = info.location || {};
  const address = location.text || null;
  if (address) {
    profile.addresses = [{ value: address, type: 'home' }];
  }

  profile.notes = data.text;

  // Social media
  function getSocialMediaUrls(): Array<Record<string, any>> {
    const urls = info.urls || [];
    const websiteList: Array<Record<string, any>> = [];
    for (const url of urls) {
      if (typeof url === 'object' && url.url && url.url.trim() !== '') {
        websiteList.push({ value: url.url });
      }
    }
    return websiteList;
  }

  const socialMediaUrls = getSocialMediaUrls();
  if (socialMediaUrls.length > 0) {
    profile.social_media_addresses = socialMediaUrls;
  }

  // Employment history
  const experiences = data.experiences || [];
  if (experiences.length > 0) {
    const lastExperience = experiences[0];
    profile.company = lastExperience.company;
    profile.title = lastExperience.title;
    profile.employments = [];

    for (const experience of experiences) {
      if (experience.title && experience.company && experience.date_start) {
        profile.employments.push({
          company_name: experience.company,
          title: experience.title,
          start_date: experience.date_start,
          end_date: experience.date_end,
        });
      }
    }
  }

  return profile;
}

export function formatToHrflowProfile(data: Record<string, any>): Record<string, any> {
  /**
   * Format a profile greenhouse object to a hrflow profile object
   * Args:
   *   profile(GreenhouseProfileModel): profile object in the greenhouse profile format
   * Returns:
   *   HrFlowProfile: profile in the hrflow profile format
   */
  const profile: Record<string, any> = {};
  profile.reference = data.id;

  // Find home address
  const addresses = data.addresses || [];
  const homeAddress = addresses.find((addr: any) => addr.type === 'home')?.value || null;

  // Extract email and phone
  const emailAddresses = data.email_addresses || [];
  const email = emailAddresses.length > 0 ? emailAddresses[0].value : null;

  const phoneNumbers = data.phone_numbers || [];
  const phone = phoneNumbers.length > 0 ? phoneNumbers[0].value : null;

  // Build info object
  profile.info = {
    first_name: data.first_name,
    last_name: data.last_name,
    email: email,
    phone: phone,
    location: {
      text: homeAddress,
      lat: null,
      lng: null,
    },
  };

  profile.text = data.notes;

  // Attachments
  const attachments = data.attachments || [];
  profile.attachments = attachments.map((att: any) => ({
    public_url: att.url,
    type: att.type,
  }));

  // Experiences
  const employments = data.employments || [];
  profile.experiences = employments.map((emp: any) => ({
    title: emp.title,
    company: emp.company_name,
    date_start: emp.start_date,
    date_end: emp.end_date,
  }));

  // Educations
  const educations = data.educations || [];
  profile.educations = educations.map((edu: any) => ({
    school: edu.school_name,
    title: `${edu.degree} ${edu.discipline}`,
    date_start: edu.start_date,
    date_end: edu.end_date,
  }));

  return profile;
}

// Connector Instance
export const Greenhouse = new Connector({
  name: 'Greenhouse',
  type: ConnectorType.ATS,
  subtype: 'greenhouse',
  description: '',
  url: 'https://www.greenhouse.io/',
  actions: [
    new ConnectorAction({
      name: ActionName.pull_job_list,
      triggerType: WorkflowType.pull,
      description:
        'Retrieves all jobs of a board via the ***Greenhouse*** API and send them to a ***Hrflow.ai Board***.',
      parameters: BaseActionParameters.withDefaults(
        'ReadJobsActionParameters',
        formatJob
      ),
      origin: GreenhouseJobWarehouse,
      target: HrFlowJobWarehouse as any,
      actionType: ActionType.inbound,
    }),
    new ConnectorAction({
      name: ActionName.push_profile,
      triggerType: WorkflowType.catch,
      description:
        'Writes a profile from Hrflow.ai Source to Greenhouse via the API for the given job_id(s) provided in tags.',
      parameters: BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatProfile
      ),
      origin: HrFlowProfileWarehouse as any,
      target: GreenhouseProfileWarehouse,
      actionType: ActionType.outbound,
    }),
    new ConnectorAction({
      name: ActionName.pull_profile_list,
      triggerType: WorkflowType.pull,
      description:
        'Retrieves all profiles via the ***Greenhouse*** API and send them to a ***Hrflow.ai Board***.',
      parameters: BaseActionParameters.withDefaults(
        'ReadProfilesActionParameters',
        formatToHrflowProfile
      ),
      origin: GreenhouseProfileWarehouse,
      target: HrFlowProfileWarehouse as any,
      actionType: ActionType.inbound,
    }),
  ],
});

export default Greenhouse;
