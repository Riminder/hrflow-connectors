/**
 * Documentation module - TypeScript translation of documentation.py (419 lines)
 * Handles generation of documentation for connectors
 */

import path from 'path';
import { ALL_TARGET_CONNECTORS_LIST_PATH } from './common';

// ===== CONSTANTS  =====

const CONNECTORS_DIRECTORY = path.join(__dirname, '..', 'connectors');

const ACTIONS_SECTIONS_REGEXP = 
  /# 🔌 Connector Actions.+?\|\s*Action\s*\|\s*Description\s*\|.+?\|\s+?<\/p>/;

const GIT_UPDATE_EXCLUDE_PATTERN = 
  /(notebooks\/\.gitkeep|mappings\/format\/\.gitkeep|README\.md|test\-config\.yaml|logo\.png|docs\/)/;

const GIT_UPDATE_TIMEOUT = 5000;

const GIT_UPDATE_DATE = `
git ls-tree -r --name-only HEAD {base_connector_path}/{connector} | while read filename; do
  echo "$(git log -1 --grep "[v1-v2-migration]" --invert-grep --format="%cI" -- $filename) $filename"
done
`;

const HRFLOW_CONNECTORS_REMOTE_URL = 'https://github.com/Riminder/hrflow-connectors';

const PREMIUM_STATUS = ':lock: Premium';
const PREMIUM_README_LINK = 'https://forms.gle/pokoE9pAjSVSFtCe7';
const OPENSOURCE_STATUS = ':book: Open source';

// ===== ENUMS & TYPES =====

export interface TemplateField {
  name: string;
  type: string;
  required: boolean;
  description: string;
  example: string;
  default: string;
}

export interface FieldInfo {
  default?: any;
  default_factory?: () => any;
  description?: string;
  outer_type_?: any;
  name: string;
  required: boolean;
  field_info?: {
    const?: boolean;
    description?: string;
    extra?: Record<string, any>;
  };
}

// ===== HELPER FUNCTIONS =====

/**
 * Extract example value from field
 */
export function fieldExample(field: FieldInfo): string {
  if (typeof field.default === 'function') {
    return 'lambda *args, **kwargs: None # Put your code logic here';
  }

  if (field.default !== undefined && field.default !== null) {
    if (typeof field.default === 'string') {
      return `"${field.default}"`;
    }
    return String(field.default);
  }

  if (field.default_factory) {
    return String(field.default_factory());
  }

  return '***';
}

/**
 * Extract default value from field
 */
export function fieldDefault(field: FieldInfo, documentationPath: string): string {
  if (typeof field.default === 'function') {
    const code = field.default as any;
    const filepath = code?.__code__?.co_filename || '';
    const relPath = path.relative(documentationPath, filepath);
    
    if (filepath.includes('site-packages/hrflow_connectors/')) {
      return `[\`${code?.name || 'function'}\`](${filepath}#L${code?.__code__?.co_firstlineno || 0})`;
    }
    return `[\`${code?.name || 'function'}\`](${relPath}#L${code?.__code__?.co_firstlineno || 0})`;
  }

  if (field.default_factory) {
    return String(field.default_factory());
  }

  return field.default !== undefined ? String(field.default) : '';
}

/**
 * Extract type from field
 */
export function fieldType(field: FieldInfo): string {
  const outerType = field.outer_type_;

  if (outerType === String || outerType === Number || outerType === Boolean) {
    return (outerType as any).name.toLowerCase();
  }

  if (typeof outerType === 'object' && outerType?.name) {
    return outerType.name;
  }

  return String(outerType);
}

/**
 * Get template fields from field definitions
 */
export function getTemplateFields(
  fields: FieldInfo[],
  documentationPath: string
): TemplateField[] {
  return fields
    .filter(field => 
      !field.field_info?.const && 
      field.field_info?.extra?.skip_from_docs !== true
    )
    .map(field => ({
      name: field.name,
      type: fieldType(field),
      required: field.required,
      description: field.field_info?.description || '',
      example: fieldExample(field),
      default: fieldDefault(field, documentationPath),
    }));
}

/**
 * Apply Python 3.7-3.8 compatibility patch to typing strings
 */
export function py37_38CompatPatch(content: string): string {
  return content.replace(
    /Union\[([\w\.]+), NoneType\]/g,
    (match, type) => `Optional[${type}]`
  );
}

/**
 * Ensure gitkeep exists in directories
 */
export function ensureGitkeep(directory: string, gitkeepFilename: string = '.gitkeep'): void {
  const fs = require('fs');
  const fsSync = require('fs');

  const gitkeepPath = path.join(directory, gitkeepFilename);
  let createEmptyFile = true;

  try {
    if (fsSync.statSync(directory).isDirectory()) {
      const files = fsSync.readdirSync(directory);
      for (const file of files) {
        if (file !== gitkeepFilename) {
          createEmptyFile = false;
          try {
            fsSync.unlinkSync(gitkeepPath);
          } catch {}
          break;
        }
      }
    }
  } catch {
    if (createEmptyFile) {
      fsSync.mkdirSync(directory, { recursive: true });
    }
  }

  if (createEmptyFile) {
    try {
      fsSync.writeFileSync(gitkeepPath, '');
    } catch {}
  }
}

// ===== DOCUMENTATION GENERATION =====

export interface ConnectorInfo {
  name: string;
  subtype: string;
  type?: string;
  description?: string;
  pre_v2_updated_at?: string;
  release_date?: string;
}

export interface ConnectorObject {
  name: string;
  subtype: string;
  type: string;
  description?: string;
  actions?: Array<{
    name: string;
    description: string;
  }>;
}

/**
 * Update root README documentation
 */
export function updateRootReadme(
  connectors: ConnectorObject[],
  targetConnectors: ConnectorInfo[],
  rootPath: string,
  rootTemplate: string
): Record<string, any> {
  const connectorByName = new Map<string, ConnectorObject>();
  for (const connector of connectors) {
    connectorByName.set(connector.name, connector);
  }

  const allConnectors = targetConnectors
    .map((connector) => ({
      ...connector,
      object: connectorByName.get(connector.name),
    }))
    .sort((a, b) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));

  let opensourceConnectorsTable = '';
  let opensourceJobboardsTable = '';
  let premiumConnectorsTable = '';
  let premiumJobboardsTable = '';

  for (const connector of allConnectors) {
    const linePattern = 
      `| [**${connector.name}**]({readme_link}) | ${connector.type} | {status} | {release_date} | {updated_at} |`;

    if (!connector.object) {
      const updated = linePattern
        .replace('{readme_link}', PREMIUM_README_LINK)
        .replace('{status}', PREMIUM_STATUS)
        .replace('{release_date}', '')
        .replace('{updated_at}', '');

      if (connector.type === 'Job Board') {
        premiumJobboardsTable += updated + '\n';
      } else {
        premiumConnectorsTable += updated + '\n';
      }
    } else {
      const updated = linePattern
        .replace('{readme_link}', `./${connector.subtype}/README.md`)
        .replace('{status}', OPENSOURCE_STATUS)
        .replace('{release_date}', `*${connector.release_date || ''}*`)
        .replace('{updated_at}', `*${connector.pre_v2_updated_at || ''}*`);

      if (connector.type === 'Job Board') {
        opensourceJobboardsTable += updated + '\n';
      } else {
        opensourceConnectorsTable += updated + '\n';
      }
    }
  }

  return {
    opensource_connectors_table: opensourceConnectorsTable.trim(),
    opensource_jobboards_table: opensourceJobboardsTable.trim(),
    premium_connectors_table: premiumConnectorsTable.trim(),
    premium_jobboards_table: premiumJobboardsTable.trim(),
  };
}

/**
 * Generate documentation for connectors
 */
export async function generateDocs(
  connectors: ConnectorObject[],
  targetConnectors?: ConnectorInfo[],
  connectorsDirectory: string = CONNECTORS_DIRECTORY,
  rootTemplate?: string
): Promise<void> {
  if (!targetConnectors) {
    const fs = require('fs');
    try {
      const data = fs.readFileSync(ALL_TARGET_CONNECTORS_LIST_PATH, 'utf-8');
      targetConnectors = JSON.parse(data);
    } catch {
      targetConnectors = [];
    }
  }

  const readmeData = updateRootReadme(
    connectors,
    targetConnectors,
    path.dirname(path.dirname(path.dirname(path.dirname(connectorsDirectory)))),
    rootTemplate || ''
  );

  let readmeContent = rootTemplate || '';
  for (const [key, value] of Object.entries(readmeData)) {
    readmeContent = readmeContent.replace(`{{ ${key} }}`, String(value));
  }

  readmeContent = py37_38CompatPatch(readmeContent);

  const readmePath = path.join(
    path.dirname(path.dirname(path.dirname(path.dirname(connectorsDirectory)))),
    'README.md'
  );

  const fs = require('fs');
  try {
    fs.writeFileSync(readmePath, readmeContent);
  } catch {}

  // Generate connector-specific documentation
  for (const connector of connectors) {
    const connectorDirectory = path.join(connectorsDirectory, connector.subtype);

    try {
      const notebooksDir = path.join(connectorDirectory, 'notebooks');
      ensureGitkeep(notebooksDir);

      const formatMappingsDir = path.join(connectorDirectory, 'mappings', 'format');
      ensureGitkeep(formatMappingsDir);

      if (connector.actions && connector.actions.length > 0) {
        const actionDocsDir = path.join(connectorDirectory, 'docs');
        try {
          fs.mkdirSync(actionDocsDir, { recursive: true });
        } catch {}

        for (const action of connector.actions) {
          const actionFile = path.join(actionDocsDir, `${action.name}.md`);
          const actionDoc = `# ${action.name}\n\n${action.description || ''}\n`;
          try {
            fs.writeFileSync(actionFile, actionDoc);
          } catch {}
        }
      }
    } catch {}
  }
}

/**
 * Check if connector is V2
 */
export function connectorIsV2(connector: any): boolean {
  return connector.constructor.name === 'ConnectorV2';
}

/**
 * Validate connector README format
 */
export function validateConnectorReadmeFormat(content: string): boolean {
  return ACTIONS_SECTIONS_REGEXP.test(content);
}

export default {
  fieldExample,
  fieldDefault,
  fieldType,
  getTemplateFields,
  py37_38CompatPatch,
  ensureGitkeep,
  updateRootReadme,
  generateDocs,
  connectorIsV2,
  validateConnectorReadmeFormat,
  CONNECTORS_DIRECTORY,
  ACTIONS_SECTIONS_REGEXP,
  GIT_UPDATE_EXCLUDE_PATTERN,
  GIT_UPDATE_TIMEOUT,
  HRFLOW_CONNECTORS_REMOTE_URL,
};
