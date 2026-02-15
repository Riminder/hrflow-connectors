/**
 * Bullhorn Connector
 * Complete TypeScript translation of connector.py
 */

import axios from 'axios';
import {
  ActionName,
  ActionType,
  BaseActionParameters,
  Connector,
  ConnectorAction,
  ConnectorType,
  WorkflowType,
} from '../../core';
import { BullhornProfile, HrFlowProfile } from './schemas';
import {
  BullhornProfileWarehouse,
  BullhornApplicationWarehouse,
  BullhornProfileParsingWarehouse,
  BullhornJobWarehouse,
} from './warehouse';

/**
 * Convert value to integer, returning 0 if null
 */
function toInt(elm: any): number {
  if (elm === null || elm === undefined) {
    return 0;
  }
  return parseInt(String(elm), 10);
}

/**
 * Extract location information from profile info
 */
function getLocation(info: Record<string, any> | null): Record<string, any> | null {
  if (info === null || info === undefined) {
    return null;
  }

  const location = info.location || {};
  const fields = location.fields || (Array.isArray(location.fields) ? {} : location.fields);

  const locationDict = {
    address1: location.text || null,
    address2: null,
    city: fields.city || null,
    state: fields.country || null,
    zip: fields.postcode || null,
  };

  return locationDict;
}

/**
 * Extract and format skills from profile data
 */
function getSkills(data: Record<string, any>): string {
  let skills = '';

  if (data?.skills && Array.isArray(data.skills) && data.skills.length > 0) {
    const skillNames = data.skills.map((skill: any) => skill.name);
    skills = skillNames.join(', ');
  }

  return skills;
}

/**
 * Transform HrFlow education data to Bullhorn format
 */
function getEducation(
  educationList: Array<Record<string, any>>
): Array<Record<string, any>> {
  const educations: Array<Record<string, any>> = [];

  for (const hrflowEducation of educationList) {
    const location = hrflowEducation.location;
    const startDate = hrflowEducation.date_start
      ? Math.floor(new Date(hrflowEducation.date_start).getTime() / 1000)
      : null;
    const endDate = hrflowEducation.date_end
      ? Math.floor(new Date(hrflowEducation.date_end).getTime() / 1000)
      : null;

    const education = {
      id: '0',
      candidate: { id: null },
      school: hrflowEducation.school || null,
      degree: hrflowEducation.title || null,
      comments: hrflowEducation.description || null,
      city: location?.text || null,
      startDate,
      endDate,
    };

    educations.push(education);
  }

  return educations;
}

/**
 * Transform HrFlow experience data to Bullhorn format
 */
function getExperience(
  experienceList: Array<Record<string, any>>
): Array<Record<string, any>> {
  const experienceJson: Array<Record<string, any>> = [];

  for (const hrflowExperience of experienceList) {
    const startDate = hrflowExperience.date_start
      ? Math.floor(new Date(hrflowExperience.date_start).getTime() / 1000)
      : null;
    const endDate = hrflowExperience.date_end
      ? Math.floor(new Date(hrflowExperience.date_end).getTime() / 1000)
      : null;

    const experience = {
      id: '0',
      candidate: { id: null },
      companyName: hrflowExperience.company || null,
      title: hrflowExperience.title || null,
      comments: hrflowExperience.description || null,
      startDate,
      endDate,
    };

    experienceJson.push(experience);
  }

  return experienceJson;
}

/**
 * Transform HrFlow attachments to Bullhorn format
 */
async function getAttachments(
  attachmentList: Array<Record<string, any>>,
  fileType: string = 'SAMPLE',
  contentType: string = 'text/plain',
  attachmentType: string = 'cover',
  format: boolean = false
): Promise<Array<Record<string, any>>> {
  const attachmentsJson: Array<Record<string, any>> = [];

  for (const hrflowAttachment of attachmentList) {
    const url = hrflowAttachment.public_url;

    try {
      const response = await axios.get(url, { responseType: 'arraybuffer' });
      const b64 = Buffer.from(response.data).toString('base64');

      const attachment: Record<string, any> = {
        externalID: 'portfolio',
        fileContent: b64,
        fileType,
        name: hrflowAttachment.file_name,
        description: 'Resume file for candidate.',
        type: attachmentType,
      };

      if (format) {
        attachment.format = 'PDF';
      } else {
        attachment.contentType = contentType;
      }

      attachmentsJson.push(attachment);
    } catch (error) {
      console.error(`Failed to fetch attachment from ${url}: ${error}`);
    }
  }

  return attachmentsJson;
}

/**
 * Format HrFlow profile to Bullhorn profile format
 */
async function formatProfile(data: HrFlowProfile): Promise<Record<string, any>> {
  const info = data.info || {};

  let dateOfBirth: number | null = null;
  if (info?.date_birth) {
    const dateObj = new Date(info.date_birth);
    dateOfBirth = Math.floor(dateObj.getTime() / 1000);
  }

  const createProfileBody = {
    id: data.reference || null,
    address: getLocation(info),
    certifications: null,
    name: info.full_name || null,
    firstName: info.first_name || null,
    lastName: info.last_name || null,
    email: info.email || null,
    mobile: info.phone || null,
    dateOfBirth,
    experience: toInt(data.experiences_duration),
    skillSet: getSkills(data),
  };

  const enrichProfileEducation = getEducation(data.educations || []);
  const enrichProfileExperience = getExperience(data.experiences || []);
  const enrichProfileAttachment = await getAttachments(data.attachments || []);

  const profileBodyDict = {
    create_profile_body: createProfileBody,
    enrich_profile_education: enrichProfileEducation,
    enrich_profile_experience: enrichProfileExperience,
    enrich_profile_attachment: enrichProfileAttachment,
  };

  return profileBodyDict;
}

/**
 * Format Bullhorn job to HrFlow job format
 */
function formatJob(data: Record<string, any>): Record<string, any> {
  const hrflowName = data.title || null;
  const hrflowRef = String(data.id || '');

  const address = data.address || {};
  const hrflowFields = {
    city: address.city || null,
    country: address.state || null,
    postal_code: address.zip || null,
  };

  const hrflowLocation = {
    text: address.address1 || null,
    fields: hrflowFields,
  };

  const sectionDescription = {
    name: 'Bullhorn_description',
    title: 'Bullhorn_description',
    description: data.publicDescription || '',
  };

  const hrflowSections = [sectionDescription];

  let degreeList = data.degreeList;
  if (degreeList && Array.isArray(degreeList)) {
    degreeList = degreeList.join(', ');
  }

  const tags: Array<Record<string, any>> = [
    { name: 'durationWeeks', value: data.durationWeeks || null },
    { name: 'degreeList', value: degreeList || null },
    { name: 'employmentType', value: data.employmentType || null },
    { name: 'numOpenings', value: data.numOpenings || null },
    { name: 'onSite', value: data.onSite || null },
    { name: 'salaryUnit', value: data.salaryUnit || null },
    { name: 'startDate', value: data.startDate || null },
    { name: 'status', value: data.status || null },
    { name: 'type', value: data.type || null },
    { name: 'willRelocate', value: data.willRelocate || null },
    { name: 'salary', value: data.salary || null },
    { name: 'isWorkFromHome', value: data.isWorkFromHome || null },
    { name: 'hoursPerWeek', value: data.hoursPerWeek || null },
    { name: 'hoursOfOperation', value: data.hoursOfOperation || null },
    { name: 'dateAdded', value: data.dateAdded || null },
  ];

  const hrflowSkills: Array<Record<string, any>> = [];
  let skillList = data.skillList;

  if (skillList) {
    if (typeof skillList === 'string') {
      skillList = skillList.split(',');
    }

    if (Array.isArray(skillList)) {
      for (const skill of skillList) {
        const newSkill = {
          name: skill.trim(),
          type: 'undefined',
          value: null,
        };
        hrflowSkills.push(newSkill);
      }
    }
  }

  const hrflowJob = {
    name: hrflowName,
    reference: hrflowRef,
    location: hrflowLocation,
    sections: hrflowSections,
    skills: hrflowSkills,
    tags,
  };

  return hrflowJob;
}

/**
 * Format Bullhorn profile to HrFlow profile with parsing
 */
function profileFormatParsing(data: BullhornProfile): Record<string, any> {
  const profile: Record<string, any> = {};

  const tags: Array<Record<string, any>> = [
    { name: 'dateAvailable', value: data.dateAvailable || null },
    { name: 'status', value: data.status || null },
    { name: 'employeeType', value: data.employeeType || null },
    {
      name: 'activePlacements',
      value: data.activePlacements?.total || null,
    },
  ];

  profile.reference = String(data.id || '');
  profile.tags = tags;
  profile.metadatas = [];
  profile.created_at = null;

  if (data.cvFile) {
    profile.resume = {
      raw: data.cvFile,
      content_type: 'application/pdf',
    };
  }

  return profile;
}

/**
 * Format Bullhorn profile to HrFlow profile
 */
function profileFormat(data: BullhornProfile): Record<string, any> {
  const firstName = data.firstName || null;
  const lastName = data.lastName || null;
  const fullName = data.name || null;
  const email = data.email || null;
  const phone = data.mobile || null;
  const dateBirth = data.dateOfBirth || null;
  const gender = data.gender || null;

  const address = data.address || {};
  const locationText = address.address1 || null;
  const location = { text: locationText };

  const info = {
    full_name: fullName,
    first_name: firstName,
    last_name: lastName,
    email,
    phone,
    date_birth: dateBirth,
    location,
    gender,
  };

  const tags: Array<Record<string, any>> = [
    { name: 'dateAvailable', value: data.dateAvailable || null },
    { name: 'status', value: data.status || null },
    { name: 'employeeType', value: data.employeeType || null },
    {
      name: 'activePlacements',
      value: data.activePlacements?.total || null,
    },
  ];

  const hrflowSkills: Array<Record<string, any>> = [];
  let skillList = data.skillSet;

  if (skillList) {
    if (typeof skillList === 'string') {
      skillList = skillList.split(',');
    }

    if (Array.isArray(skillList)) {
      for (const skill of skillList) {
        const newSkill = {
          name: skill.trim(),
          type: 'hard',
          value: null,
        };
        hrflowSkills.push(newSkill);
      }
    }
  }

  const hrflowEducation: Array<Record<string, any>> = [];
  const educations = data.educations || [];

  for (const education of educations) {
    const educLocation = {
      text: education.city || null,
      lng: null,
      lat: null,
    };

    const school = education.school || null;
    const dateStart = education.startDate || null;
    const dateEnd = education.endDate || null;
    const title = education.degree || null;
    const certifications = education.certification ? [education.certification] : [];
    const description = education.comments || null;

    const objectEducation = {
      location: educLocation,
      school,
      date_start: dateStart,
      date_end: dateEnd,
      title,
      certifications,
      description,
    };

    hrflowEducation.push(objectEducation);
  }

  const hrflowExperience: Array<Record<string, any>> = [];
  const workHistories = data.workHistories || [];

  for (const experience of workHistories) {
    const expLocation = {
      text: '',
      lng: null,
      lat: null,
    };

    const company = experience.companyName || null;
    const dateStart = experience.startDate || null;
    const dateEnd = experience.endDate || null;
    const expTitle = experience.title || null;
    const description = experience.comments || null;

    const objectExperience = {
      title: expTitle,
      location: expLocation,
      company,
      date_start: dateStart,
      date_end: dateEnd,
      description,
    };

    hrflowExperience.push(objectExperience);
  }

  const profile = {
    info,
    skills: hrflowSkills,
    experiences: hrflowExperience,
    tags,
    educations: hrflowEducation,
  };

  profile.reference = String(data.id || '');

  return profile;
}

/**
 * Format application data for submission
 */
async function formatApplication(data: HrFlowProfile): Promise<Record<string, any>> {
  const info = data.info || {};
  const metadatas = data.metadatas || [];

  const attachments = data.attachments ? [data.attachments[0]] : [];

  const comment = metadatas.find(
    (metadata: Record<string, any>) => metadata.name === 'comment'
  )?.value || null;

  const profile: Record<string, any> = {
    firstName: info.first_name || null,
    lastName: info.last_name || null,
    name: info.full_name || null,
    address: getLocation(info),
    email: info.email || null,
    mobile: info.phone || null,
    comment,
  };

  const attachmentList = await getAttachments(
    attachments,
    'RESUME',
    'application/pdf',
    'RESUME',
    true
  );

  profile.attachment = attachmentList.length > 0 ? attachmentList[0] : {};

  return profile;
}

const DESCRIPTION =
  'Transform Your Business with Bullhorn Staffing and Recruitment Software';

/**
 * Bullhorn Connector - Main connector definition
 */
export const Bullhorn = new Connector(
  'Bullhorn',
  ConnectorType.ATS,
  'bullhorn',
  DESCRIPTION,
  'https://www.bullhorn.com/',
  [
    new ConnectorAction(
      ActionName.push_profile,
      WorkflowType.catch,
      'Writes a profile from Hrflow.ai Source to Bullhorn via the API',
      BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatProfile
      ),
      { name: 'HrFlowProfileWarehouse' },
      BullhornProfileWarehouse,
      ActionType.outbound
    ),
    new ConnectorAction(
      ActionName.pull_job_list,
      WorkflowType.pull,
      'Retrieves jobs from Bullhorn and writes them to Hrflow.ai Board',
      BaseActionParameters.withDefaults('ReadJobsActionParameters', formatJob),
      BullhornJobWarehouse,
      { name: 'HrFlowJobWarehouse' },
      ActionType.inbound
    ),
    new ConnectorAction(
      ActionName.pull_resume_attachment_list,
      WorkflowType.pull,
      'retrieves profiles attachments from Bullhorn and Parses them and sends them to Hrflow.ai source',
      BaseActionParameters.withDefaults(
        'ReadProfileActionParameters',
        profileFormatParsing
      ),
      BullhornProfileParsingWarehouse,
      { name: 'HrFlowProfileParsingWarehouse' },
      ActionType.inbound
    ),
    new ConnectorAction(
      ActionName.pull_profile_list,
      WorkflowType.pull,
      'Retrieves profiles from Bullhorn and writes them to Hrflow.ai source',
      BaseActionParameters.withDefaults(
        'ReadProfileActionParameters',
        profileFormat
      ),
      BullhornProfileWarehouse,
      { name: 'HrFlowProfileWarehouse' },
      ActionType.inbound
    ),
    new ConnectorAction(
      ActionName.push_application,
      WorkflowType.catch,
      'Retrieves profiles from Hrflow.ai and writes their applications to the Bullhorn source',
      BaseActionParameters.withDefaults(
        'WriteProfileActionParameters',
        formatApplication
      ),
      { name: 'HrFlowProfileWarehouse' },
      BullhornApplicationWarehouse,
      ActionType.outbound
    ),
  ]
);

export default Bullhorn;
