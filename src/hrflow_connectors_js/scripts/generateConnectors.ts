/**
 * Connector Generator Script
 * Generates TypeScript connectors from Python connector definitions
 */

import fs from 'fs';
import path from 'path';
import { execSync } from 'child_process';

interface ConnectorConfig {
  name: string;
  pythonPath: string;
  subtype: string;
  type: string;
}

const CONNECTORS_TO_GENERATE: ConnectorConfig[] = [
  {
    name: 'adzuna',
    pythonPath: '/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors/adzuna',
    subtype: 'adzuna',
    type: 'CLASSIFIEDS',
  },
  {
    name: 'ashby',
    pythonPath: '/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors/ashby',
    subtype: 'ashby',
    type: 'ATS',
  },
  {
    name: 'greenhouse',
    pythonPath: '/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors/greenhouse',
    subtype: 'greenhouse',
    type: 'ATS',
  },
  // ... more connectors can be added here
];

/**
 * Generate a TypeScript connector template
 */
function generateConnectorTemplate(config: ConnectorConfig): string {
  return `/**
 * ${config.name} Connector
 * Auto-generated TypeScript translation
 */

import {
  ActionName,
  ConnectorType,
  WorkflowType,
  BaseActionParameters,
} from '../../core';
import { Connector, ConnectorAction } from '../../core/connector';

/**
 * Create the ${config.name} connector
 */
export const ${capitalizeFirstLetter(config.name)} = new Connector(
  '${capitalizeFirstLetter(config.name)}',
  ConnectorType.${config.type},
  '${config.subtype}',
  '${config.name} Connector',
  'https://example.com',
  [
    // TODO: Add connector actions here
  ],
);

export default ${capitalizeFirstLetter(config.name)};
`;
}

function capitalizeFirstLetter(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Create a connector directory structure
 */
function createConnectorStructure(config: ConnectorConfig): void {
  const basePath = `/workspaces/hrflow-connectors/src/hrflow_connectors_js/v1/connectors/${config.name}`;

  // Create directories
  if (!fs.existsSync(basePath)) {
    fs.mkdirSync(basePath, { recursive: true });
  }

  // Create index.ts
  const indexPath = path.join(basePath, 'index.ts');
  if (!fs.existsSync(indexPath)) {
    fs.writeFileSync(indexPath, generateConnectorTemplate(config));
  }

  // Create schemas.ts
  const schemasPath = path.join(basePath, 'schemas.ts');
  if (!fs.existsSync(schemasPath)) {
    fs.writeFileSync(
      schemasPath,
      `/**\n * ${config.name} Connector - Schemas\n */\n\nexport interface ${capitalizeFirstLetter(config.name)}Profile {\n  id?: string;\n  // TODO: Add fields here\n}\n`,
    );
  }

  // Create warehouse.ts
  const warehousePath = path.join(basePath, 'warehouse.ts');
  if (!fs.existsSync(warehousePath)) {
    fs.writeFileSync(
      warehousePath,
      `/**\n * ${config.name} Connector - Warehouse\n */\n\nimport { Warehouse } from '../../core/warehouse';\n\n// TODO: Implement warehouse definitions\n`,
    );
  }

  console.log(`Created connector structure for ${config.name} at ${basePath}`);
}

/**
 * Generate all connectors
 */
export function generateAllConnectors(): void {
  console.log('Starting connector generation...');

  for (const config of CONNECTORS_TO_GENERATE) {
    try {
      createConnectorStructure(config);
      console.log(`✓ Generated ${config.name}`);
    } catch (error) {
      console.error(`✗ Failed to generate ${config.name}: ${error}`);
    }
  }

  console.log('Connector generation complete!');
}

// Run generator if executed directly
if (require.main === module) {
  generateAllConnectors();
}

export default generateAllConnectors;
