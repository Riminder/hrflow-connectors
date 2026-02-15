/**
 * Common module - Constants and utilities
 * TypeScript translation of common.py
 */

import path from 'path';

/**
 * Path to the JSON file containing list of all target connectors
 */
const ALL_TARGET_CONNECTORS_LIST_PATH = path.join(
  __dirname,
  '..',
  '..',
  'data',
  'connectors.json'
);

export { ALL_TARGET_CONNECTORS_LIST_PATH };
