/**
 * Example: Using the Bullhorn Connector
 */

import pino from 'pino';
import { Bullhorn } from './../v1/connectors/bullhorn';
import { Status, Event } from './../v1/core';

const logger = pino();

async function example() {
  logger.info('Starting Bullhorn connector example...');

  // Initialize connector
  const bullhorn = new Bullhorn();

  // Example: Pull jobs from Bullhorn
  try {
    const result = await bullhorn.pull_job_list(
      'workflow-123',
      {
        logics: [
          // Filter jobs with salary
          (item) => {
            if (!item.salary) {
              return null; // Discard items without salary
            }
            return item;
          },
        ],
        format: (item) => {
          // Transform job structure
          return {
            id: item.id,
            title: item.title,
            description: item.publicDescription,
            location: item.address?.city,
            salary: item.salary,
          };
        },
      },
      {
        // Origin (Bullhorn) parameters
        clientId: process.env.BULLHORN_CLIENT_ID || '',
        clientSecret: process.env.BULLHORN_CLIENT_SECRET || '',
        username: process.env.BULLHORN_USERNAME || '',
        password: process.env.BULLHORN_PASSWORD || '',
        lastModifiedDate: '0',
        fields: '*',
        query: 'isDeleted:0',
        count: 100,
      },
      {
        // Target (HrFlow) parameters
        // These would configure HrFlow.ai target
      },
    );

    logger.info(`Pull jobs result: ${result.status}`);
    logger.info(`Read: ${result.events[Event.READ_SUCCESS]}, Failed: ${result.events[Event.READ_FAILURE]}`);
    logger.info(`Format failures: ${result.events[Event.FORMAT_FAILURE]}`);
    logger.info(`Write failures: ${result.events[Event.WRITE_FAILURE]}`);

    if (result.status === Status.SUCCESS) {
      logger.info('✓ Successfully pulled and processed jobs');
    } else if (result.status === Status.SUCCESS_WITH_FAILURES) {
      logger.warn('⚠ Completed with some failures');
    } else {
      logger.error('✗ Failed with reason: ' + result.reason);
    }
  } catch (error) {
    logger.error('Error pulling jobs:', error);
  }

  // Example: Push profile to Bullhorn
  try {
    const result = await bullhorn.push_profile(
      'workflow-456',
      {
        format: (item) => {
          // Transform HrFlow profile to Bullhorn format
          return {
            create_profile_body: {
              id: item.reference,
              name: item.info?.full_name,
              firstName: item.info?.first_name,
              lastName: item.info?.last_name,
              email: item.info?.email,
              mobile: item.info?.phone,
            },
            enrich_profile_education: item.educations || [],
            enrich_profile_experience: item.experiences || [],
            enrich_profile_attachment: item.attachments || [],
          };
        },
      },
      {
        // Origin (HrFlow) parameters
      },
      {
        // Target (Bullhorn) parameters
        clientId: process.env.BULLHORN_CLIENT_ID || '',
        clientSecret: process.env.BULLHORN_CLIENT_SECRET || '',
        username: process.env.BULLHORN_USERNAME || '',
        password: process.env.BULLHORN_PASSWORD || '',
      },
    );

    logger.info(`Push profile result: ${result.status}`);

    if (result.status === Status.SUCCESS) {
      logger.info('✓ Successfully pushed profile to Bullhorn');
    }
  } catch (error) {
    logger.error('Error pushing profile:', error);
  }
}

// Run example
example().catch(logger.error);
