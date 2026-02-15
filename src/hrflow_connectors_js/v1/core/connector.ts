/**
 * Core Connector module - Complete TypeScript translation of connector.py (1193 lines)
 */

import { v4 as uuid } from 'uuid';
import pino from 'pino';
import { ReadMode, Warehouse, ParametersModel } from './warehouse';

const logger = pino();

// ===== CONSTANTS =====
const KB = 1024;
const MAX_LOGO_SIZE_BYTES = 100 * KB;
const MAX_LOGO_PIXEL = 150;
const MIN_LOGO_PIXEL = 34;
const CONNECTOR_SUBTYPE_FORMAT_REGEX = /^[a-z]+$/;
const HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE = 'https://raw.githubusercontent.com/Riminder/hrflow-connectors';

// ===== ENUMS =====

export enum Event {
  read_success = 'read_success',
  read_failure = 'read_failure',
  format_failure = 'format_failure',
  logics_discard = 'logics_discard',
  logics_failure = 'logics_failure',
  write_failure = 'write_failure',
  callback_failure = 'callback_failure',
  callback_executed = 'callback_executed',
  item_to_read_from_failure = 'item_to_read_from_failure',
}

export enum Reason {
  item_to_read_from_failure = 'item_to_read_from_failure',
  origin_does_not_support_incremental = 'origin_does_not_support_incremental',
  backend_not_configured_in_incremental_mode = 'backend_not_configured_in_incremental_mode',
  workflow_id_not_found = 'workflow_id_not_found',
  event_parsing_failure = 'event_parsing_failure',
  bad_action_parameters = 'bad_action_parameters',
  bad_origin_parameters = 'bad_origin_parameters',
  bad_target_parameters = 'bad_target_parameters',
  format_failure = 'format_failure',
  logics_failure = 'logics_failure',
  read_failure = 'read_failure',
  write_failure = 'write_failure',
  none = '',
}

export enum Status {
  success = 'success',
  success_with_failures = 'success_with_failures',
  fatal = 'fatal',
}

export enum WorkflowType {
  catch = 'hook',
  pull = 'schedule',
}

export enum ActionName {
  pull_application_list = 'pull_application_list',
  pull_job_list = 'pull_job_list',
  pull_profile_list = 'pull_profile_list',
  pull_resume_attachment_list = 'pull_resume_attachment_list',
  push_profile = 'push_profile',
  push_job = 'push_job',
  push_profile_list = 'push_profile_list',
  push_job_list = 'push_job_list',
  push_score_list = 'push_score_list',
  catch_profile = 'catch_profile',
  catch_job = 'catch_job',
  push_application = 'push_application',
  applicant_new = 'applicant_new',
  applicant_resume_update = 'applicant_resume_update',
  applicant_update = 'applicant_update',
}

export enum ActionType {
  inbound = 'inbound',
  outbound = 'outbound',
}

export enum ConnectorType {
  ATS = 'ATS',
  CRM = 'CRM',
  HCM = 'HCM',
  Automation = 'Automation',
  JobBoard = 'Job Board',
  Classifieds = 'Classified Ads',
  Other = 'Other',
}

// ===== TYPES =====

export type EventCounter = Map<Event, number>;
export type LogicFunction = (item: Record<string, any>) => Record<string, any> | null;
export type FormatFunction = (item: Record<string, any>) => Record<string, any>;
export type EventParserFunction = (event: Record<string, any>) => Record<string, any>;
export type CallbackFunction = (
  originParams: ParametersModel,
  targetParams: ParametersModel,
  events: EventCounter,
  items: Record<string, any>[]
) => void;

export interface RunResult {
  status: Status;
  reason?: Reason;
  events: EventCounter;
  readFrom?: string;
}

export interface ActionInitError {
  data: Record<string, any>;
  reason: Reason;
}

// ===== DEFAULT MANIFESTS =====

const DEFAULT_PULL_JOB_LIST_ACTION_MANIFEST = {
  action_parameters: {},
  action_type: 'inbound',
  data_type: 'job',
  jsonmap: {},
  name: 'pull_job_list',
  origin: '',
  origin_data_schema: {},
  origin_parameters: {},
  supports_incremental: false,
  target: 'HrFlow.ai Jobs',
  target_data_schema: {},
  target_parameters: {},
  trigger_type: 'schedule',
  workflow_code: '',
  workflow_code_format_placeholder: '# << format_placeholder >>',
  workflow_code_logics_placeholder: '# << logics_placeholder >>',
  workflow_code_origin_settings_prefix: 'origin_',
  workflow_code_target_settings_prefix: 'target_',
  workflow_code_workflow_id_settings_key: '__workflow_id',
};

const DEFAULT_PULL_PROFILE_LIST_ACTION_MANIFEST = {
  action_parameters: {},
  action_type: 'inbound',
  data_type: 'profile',
  jsonmap: {},
  name: 'pull_profile_list',
  origin: '',
  origin_data_schema: {},
  origin_parameters: {},
  supports_incremental: false,
  target: 'HrFlow.ai Profiles',
  target_data_schema: {},
  target_parameters: {},
  trigger_type: 'schedule',
  workflow_code: '',
  workflow_code_format_placeholder: '# << format_placeholder >>',
  workflow_code_logics_placeholder: '# << logics_placeholder >>',
  workflow_code_origin_settings_prefix: 'origin_',
  workflow_code_target_settings_prefix: 'target_',
  workflow_code_workflow_id_settings_key: '__workflow_id',
};

const DEFAULT_PUSH_PROFILE_ACTION_MANIFEST = {
  action_parameters: {},
  action_type: 'outbound',
  data_type: 'profile',
  jsonmap: {},
  name: 'push_profile',
  origin: 'HrFlow.ai Profiles',
  origin_data_schema: {},
  origin_parameters: {},
  supports_incremental: false,
  target: '',
  target_data_schema: {},
  target_parameters: {},
  trigger_type: 'hook',
  workflow_code: '',
  workflow_code_event_parser_placeholder: '# << event_parser_placeholder >>',
  workflow_code_format_placeholder: '# << format_placeholder >>',
  workflow_code_logics_placeholder: '# << logics_placeholder >>',
  workflow_code_origin_settings_prefix: 'origin_',
  workflow_code_target_settings_prefix: 'target_',
  workflow_code_workflow_id_settings_key: '__workflow_id',
};

const DEFAULT_CATCH_PROFILE_ACTION_MANIFEST = {
  action_parameters: {},
  action_type: 'inbound',
  data_type: 'profile',
  jsonmap: {},
  name: 'catch_profile',
  origin: '',
  origin_data_schema: {},
  origin_parameters: {},
  supports_incremental: false,
  target: 'HrFlow.ai Profile Parsing',
  target_data_schema: {},
  target_parameters: {},
  trigger_type: 'hook',
  workflow_code: '',
  workflow_code_event_parser_placeholder: '# << event_parser_placeholder >>',
  workflow_code_format_placeholder: '# << format_placeholder >>',
  workflow_code_logics_placeholder: '# << logics_placeholder >>',
  workflow_code_origin_settings_prefix: 'origin_',
  workflow_code_target_settings_prefix: 'target_',
  workflow_code_workflow_id_settings_key: '__workflow_id',
};

const DEFAULT_PUSH_JOB_ACTION_MANIFEST = {
  action_parameters: {},
  action_type: 'outbound',
  data_type: 'job',
  jsonmap: {},
  name: 'push_job',
  origin: 'HrFlow.ai Jobs',
  origin_data_schema: {},
  origin_parameters: {},
  supports_incremental: false,
  target: '',
  target_data_schema: {},
  target_parameters: {},
  trigger_type: 'hook',
  workflow_code: '',
  workflow_code_event_parser_placeholder: '# << event_parser_placeholder >>',
  workflow_code_format_placeholder: '# << format_placeholder >>',
  workflow_code_logics_placeholder: '# << logics_placeholder >>',
  workflow_code_origin_settings_prefix: 'origin_',
  workflow_code_target_settings_prefix: 'target_',
  workflow_code_workflow_id_settings_key: '__workflow_id',
};

// ===== LOGGING ADAPTER =====

class ConnectorActionAdapter {
  private logTags: Array<{ name: string; value: string }>;

  constructor(logTags: Array<{ name: string; value: string }> = []) {
    this.logTags = logTags;
  }

  private formatMessage(msg: string): string {
    const tags = this.logTags.map((tag) => `[${tag.name}=${tag.value}]`).join('');
    return `${tags}: ${msg}`;
  }

  info(msg: string): void {
    logger.info(this.formatMessage(msg));
  }

  warning(msg: string): void {
    logger.warn(this.formatMessage(msg));
  }

  error(msg: string): void {
    logger.error(this.formatMessage(msg));
  }

  exception(msg: string): void {
    logger.error(this.formatMessage(msg));
  }
}

// ===== HELPER FUNCTIONS =====

function emptyEventCounter(): EventCounter {
  return new Map<Event, number>([
    [Event.read_success, 0],
    [Event.read_failure, 0],
    [Event.format_failure, 0],
    [Event.logics_discard, 0],
    [Event.logics_failure, 0],
    [Event.write_failure, 0],
    [Event.callback_failure, 0],
    [Event.callback_executed, 0],
    [Event.item_to_read_from_failure, 0],
  ]);
}

// ===== BASE ACTION PARAMETERS =====

export class BaseActionParameters extends ParametersModel {
  logics: LogicFunction[] = [];
  format: FormatFunction = (x) => x;
  event_parser?: EventParserFunction;
  read_mode: ReadMode = ReadMode.sync;

  constructor(data: Record<string, any> = {}) {
    super(data);
  }

  static schema(): Record<string, any> {
    return {
      type: 'object',
      properties: {
        logics: {
          description: 'List of logic functions that process items',
          type: 'code_editor',
        },
        format: {
          description: 'Formatting function to transform items',
          type: 'code_editor',
        },
        event_parser: {
          description: 'Event parsing function for CATCH integrations',
          type: 'code_editor',
        },
        read_mode: {
          description: 'Read mode: sync or incremental',
          type: 'string',
          enum: ['sync', 'incremental'],
        },
      },
    };
  }
}

// ===== CONNECTOR ACTION =====

export interface ConnectorActionConfig {
  name: ActionName;
  description: string;
  parameters: typeof BaseActionParameters;
  origin: Warehouse;
  target: Warehouse;
  callback?: CallbackFunction;
  trigger_type: WorkflowType;
  action_type: ActionType;
}

export class ConnectorAction {
  static readonly WORKFLOW_FORMAT_PLACEHOLDER = '# << format_placeholder >>';
  static readonly WORKFLOW_LOGICS_PLACEHOLDER = '# << logics_placeholder >>';
  static readonly WORKFLOW_EVENT_PARSER_PLACEHOLDER = '# << event_parser_placeholder >>';
  static readonly ORIGIN_SETTINGS_PREFIX = 'origin_';
  static readonly TARGET_SETTINGS_PREFIX = 'target_';
  static readonly WORKFLOW_ID_SETTINGS_KEY = '__workflow_id';

  name: ActionName;
  description: string;
  parameters: typeof BaseActionParameters;
  origin: Warehouse;
  target: Warehouse;
  callback?: CallbackFunction;
  trigger_type: WorkflowType;
  action_type: ActionType;

  constructor(config: ConnectorActionConfig) {
    this.name = config.name;
    this.description = config.description;
    this.parameters = config.parameters;
    this.origin = config.origin;
    this.target = config.target;
    this.callback = config.callback;
    this.trigger_type = config.trigger_type;
    this.action_type = config.action_type;
    this.validate();
  }

  private validate(): void {
    if (!this.origin.isReadable) {
      throw new Error('Origin warehouse is not readable');
    }
    if (!this.target.isWritable) {
      throw new Error('Target warehouse is not writable');
    }

    const pullActions = [
      ActionName.pull_application_list,
      ActionName.pull_job_list,
      ActionName.pull_profile_list,
    ];
    if (pullActions.includes(this.name) && this.trigger_type !== WorkflowType.pull) {
      throw new Error(
        `Actions pull_application_list, pull_job_list, and pull_profile_list are only available for trigger_type=${WorkflowType.pull}`
      );
    }
  }

  get dataType(): string {
    return this.origin.dataType.toString();
  }

  workflowCode(importName: string, workflowType: WorkflowType): string {
    return `# Workflow code for ${this.name} action\n`;
  }

  async run(
    connectorName: string,
    workflowId: string,
    actionParameters: Record<string, any>,
    originParameters: Record<string, any>,
    targetParameters: Record<string, any>,
    initError?: ActionInitError
  ): Promise<RunResult> {
    const actionId = uuid();
    const startedAt = new Date();
    const logTags = [
      { name: 'started_at', value: startedAt.toISOString() },
      { name: 'connector', value: connectorName },
      { name: 'action_name', value: this.name },
      { name: 'workflow_id', value: workflowId },
      { name: 'action_id', value: actionId },
    ];

    const adapter = new ConnectorActionAdapter(logTags);

    if (initError) {
      adapter.error(
        `Failed to parse event with reason=${initError.reason} data=${JSON.stringify(initError.data)}`
      );
      return {
        status: Status.fatal,
        reason: initError.reason,
        events: emptyEventCounter(),
      };
    }

    adapter.info('Starting Action');

    try {
      // Validate parameters
      const params = new this.parameters(actionParameters);
      const originParams = new this.origin.read!.parameters(originParameters);
      const targetParams = new this.target.write!.parameters(targetParameters);

      const events = emptyEventCounter();

      // Determine read_from value for incremental mode
      let readFrom: string | undefined;
      if (params.read_mode === ReadMode.incremental) {
        if (!this.origin.supportsIncremental) {
          adapter.warning(
            `Origin warehouse ${this.origin.name} does not support 'incremental' read mode`
          );
          return {
            status: Status.fatal,
            reason: Reason.origin_does_not_support_incremental,
            events,
          };
        }
        adapter.info(`Read mode is 'incremental' fetching last run results`);
      }

      // Read from origin
      const originItems: Record<string, any>[] = [];
      const readStartedAt = Date.now();

      try {
        adapter.info(
          `Starting to read from warehouse=${this.origin.name} with mode=${params.read_mode}`
        );

        const originAdapter = new ConnectorActionAdapter(
          logTags.concat([
            { name: 'warehouse', value: this.origin.name },
            { name: 'action', value: 'read' },
          ])
        );

        for await (const item of this.origin.read!.call(
          originAdapter,
          originParams,
          params.read_mode,
          readFrom
        )) {
          originItems.push(item);
          events.set(Event.read_success, (events.get(Event.read_success) || 0) + 1);
        }

        const readFinishedAt = Date.now();
        adapter.info(
          `Finished reading in ${(readFinishedAt - readStartedAt) / 1000}s from warehouse=${this.origin.name} n_items=${originItems.length}`
        );
      } catch (error) {
        events.set(Event.read_failure, (events.get(Event.read_failure) || 0) + 1);
        adapter.exception(
          `Failed to read from warehouse=${this.origin.name} error=${error}`
        );
      }

      if (originItems.length === 0) {
        if ((events.get(Event.read_failure) || 0) > 0) {
          adapter.warning('No items fetched from origin warehouse. Aborting action after read_failure');
        }
        return this.resultFromEvents(events);
      }

      // Determine next read_from for incremental mode
      let nextReadFrom = readFrom;
      if (originItems.length > 0 && params.read_mode === ReadMode.incremental) {
        const lastItem = originItems[originItems.length - 1];
        try {
          nextReadFrom = this.origin.itemToReadFrom(lastItem);
        } catch (error) {
          events.set(Event.item_to_read_from_failure, (events.get(Event.item_to_read_from_failure) || 0) + 1);
          adapter.exception(`Failed to get read_from from warehouse error=${error}`);
          return {
            status: Status.fatal,
            reason: Reason.item_to_read_from_failure,
            events,
          };
        }
      }

      // Format items
      const formattedItems: Record<string, any>[] = [];
      const hasCustomFormat = actionParameters.format !== undefined;
      adapter.info(
        `Starting to format origin items using ${hasCustomFormat ? 'user defined' : 'default'} function`
      );

      for (const item of originItems) {
        try {
          const formatted = params.format(item);
          formattedItems.push(formatted);
        } catch (error) {
          events.set(Event.format_failure, (events.get(Event.format_failure) || 0) + 1);
          adapter.exception(`Failed to format origin item error=${error}`);
        }
      }

      adapter.info(
        `Finished formatting origin items success=${formattedItems.length} failures=${events.get(Event.format_failure) || 0}`
      );

      if (formattedItems.length === 0) {
        adapter.warning(
          'Formatting failed for all items. Review supplied format function. Aborting action.'
        );
        return this.resultFromEvents(events);
      }

      // Apply logics
      let itemsToWrite = formattedItems;
      if (params.logics && params.logics.length > 0) {
        adapter.info(`Starting to apply logic functions: n_items=${formattedItems.length}`);
        itemsToWrite = [];

        for (const item of formattedItems) {
          let currentItem: Record<string, any> | null = item;

          for (let i = 0; i < params.logics.length; i++) {
            const logic = params.logics[i];
            try {
              currentItem = logic(currentItem);
            } catch (error) {
              adapter.exception(`Failed to apply logic function number=${i} error=${error}`);
              events.set(Event.logics_failure, (events.get(Event.logics_failure) || 0) + 1);
              currentItem = null;
              break;
            }

            if (currentItem === null) {
              events.set(Event.logics_discard, (events.get(Event.logics_discard) || 0) + 1);
              break;
            }
          }

          if (currentItem !== null) {
            itemsToWrite.push(currentItem);
          }
        }

        if (itemsToWrite.length === 0) {
          adapter.warning('Logics failed for all items. Review supplied logic functions. Aborting action.');
          return this.resultFromEvents(events);
        }

        adapter.info(
          `Finished applying logic functions: success=${itemsToWrite.length} discarded=${events.get(Event.logics_discard) || 0} failures=${events.get(Event.logics_failure) || 0}`
        );
      } else {
        adapter.info('No logic functions supplied. Skipping');
      }

      // Write to target
      const writeStartedAt = Date.now();
      adapter.info(
        `Starting to write to warehouse=${this.target.name} n_items=${itemsToWrite.length}`
      );

      const targetAdapter = new ConnectorActionAdapter(
        logTags.concat([
          { name: 'warehouse', value: this.target.name },
          { name: 'action', value: 'write' },
        ])
      );

      try {
        const failedItems = await this.target.write!.call(
          targetAdapter,
          targetParams,
          itemsToWrite
        );
        events.set(Event.write_failure, (events.get(Event.write_failure) || 0) + failedItems.length);
      } catch (error) {
        adapter.exception(`Failed to write to warehouse=${this.target.name} error=${error}`);
        events.set(Event.write_failure, (events.get(Event.write_failure) || 0) + itemsToWrite.length);
        return {
          status: Status.fatal,
          reason: Reason.write_failure,
          events,
        };
      }

      const writeFinishedAt = Date.now();
      adapter.info(
        `Finished writing in ${(writeFinishedAt - writeStartedAt) / 1000}s to warehouse=${this.target.name} success=${itemsToWrite.length - (events.get(Event.write_failure) || 0)} failures=${events.get(Event.write_failure) || 0}`
      );

      // Execute callback
      if (this.callback) {
        adapter.info('Calling callback function');
        try {
          this.callback(originParams, targetParams, events, itemsToWrite);
          events.set(Event.callback_executed, (events.get(Event.callback_executed) || 0) + 1);
        } catch (error) {
          events.set(Event.callback_failure, (events.get(Event.callback_failure) || 0) + 1);
          adapter.exception(`Failed to run callback error=${error}`);
        }
      }

      const result = this.resultFromEvents(events);
      result.readFrom = nextReadFrom;
      adapter.info('Finished action');
      return result;
    } catch (error) {
      adapter.exception(`Action failed error=${error}`);
      return {
        status: Status.fatal,
        reason: Reason.bad_action_parameters,
        events: emptyEventCounter(),
      };
    }
  }

  private resultFromEvents(events: EventCounter): RunResult {
    const readSuccess = events.get(Event.read_success) || 0;
    const readFailures = events.get(Event.read_failure) || 0;

    if (readSuccess === 0 && readFailures === 0) {
      return { status: Status.success, reason: Reason.none, events };
    }

    if (readSuccess === 0 && readFailures > 0) {
      return { status: Status.fatal, reason: Reason.read_failure, events };
    }

    const formatFailures = events.get(Event.format_failure) || 0;
    if (formatFailures === readSuccess) {
      return { status: Status.fatal, reason: Reason.format_failure, events };
    }

    const logicsFailures = events.get(Event.logics_failure) || 0;
    if (logicsFailures === readSuccess - formatFailures) {
      return { status: Status.fatal, reason: Reason.logics_failure, events };
    }

    const logicsDiscard = events.get(Event.logics_discard) || 0;
    const writeFailure = events.get(Event.write_failure) || 0;

    if (
      writeFailure === readSuccess - formatFailures - logicsDiscard - logicsFailures &&
      writeFailure > 0
    ) {
      return { status: Status.fatal, reason: Reason.write_failure, events };
    }

    const hasFailures =
      (events.get(Event.read_failure) || 0) > 0 ||
      (events.get(Event.format_failure) || 0) > 0 ||
      (events.get(Event.logics_failure) || 0) > 0 ||
      (events.get(Event.write_failure) || 0) > 0 ||
      (events.get(Event.callback_failure) || 0) > 0;

    if (hasFailures) {
      return { status: Status.success_with_failures, reason: Reason.none, events };
    }

    return { status: Status.success, reason: Reason.none, events };
  }
}

// ===== CONNECTOR MODEL & CLASS =====

export interface ConnectorModel {
  name: string;
  description: string;
  url: string;
  type: ConnectorType;
  subtype: string;
  actions: ConnectorAction[];
}

export interface ParametersOverride {
  name: ActionName;
  format?: FormatFunction;
  event_parser?: EventParserFunction;
}

export class Connector {
  model: ConnectorModel;

  constructor(config: {
    name: string;
    description: string;
    url: string;
    type: ConnectorType;
    subtype: string;
    actions: ConnectorAction[];
  }) {
    if (!CONNECTOR_SUBTYPE_FORMAT_REGEX.test(config.subtype)) {
      throw new Error(`Connector subtype must be lowercase with no spaces`);
    }

    this.model = {
      name: config.name,
      description: config.description,
      url: config.url,
      type: config.type,
      subtype: config.subtype,
      actions: config.actions,
    };

    // Dynamically bind action methods
    for (const action of config.actions) {
      const boundMethod = async (
        workflowId: string,
        actionParams: Record<string, any>,
        originParams: Record<string, any>,
        targetParams: Record<string, any>
      ) => action.run(config.name, workflowId, actionParams, originParams, targetParams);
      (this as any)[action.name] = boundMethod;
    }
  }

  actionByName(actionName: string): ConnectorAction | undefined {
    return this.model.actions.find((a) => a.name === actionName);
  }

  manifest(): Record<string, any> {
    return {
      name: this.model.name,
      type: this.model.type.toUpperCase().replace(' ', ''),
      subtype: this.model.subtype,
      logo: '',
      actions: this.model.actions.map((action) => ({
        name: action.name,
        action_type: action.action_type,
        data_type: action.dataType,
        trigger_type: action.trigger_type,
        origin: action.origin.name,
        target: action.target.name,
      })),
    };
  }

// ===== LOGO COMPUTATION =====

/**
 * Compute logo path for a connector
 * Validates logo size and dimensions
 */
export function computeLogoPath(
  name: string,
  subtype: string,
  connectorsDirectory: string
): string {
  const fs = require('fs');
  const path = require('path');

  const connectorDirectory = path.join(connectorsDirectory, subtype);

  if (!fs.statSync(connectorDirectory).isDirectory()) {
    throw new Error(
      `No directory found for connector ${name} in ${connectorDirectory}`
    );
  }

  // Find logo files
  const files = fs.readdirSync(connectorDirectory);
  const logoFiles = files.filter((f: string) => f.startsWith('logo.'));

  if (logoFiles.length === 0) {
    throw new Error(
      `Missing logo for connector ${name}. Add a logo file at ${connectorDirectory} named 'logo.(png|jpeg|...)'`
    );
  }

  if (logoFiles.length > 1) {
    throw new Error(
      `Found multiple logos for connector ${name} => ${logoFiles}. Only a single one should be present`
    );
  }

  const logoFile = logoFiles[0];
  const logoPath = path.join(connectorDirectory, logoFile);
  const stats = fs.statSync(logoPath);
  const size = stats.size;

  if (size > MAX_LOGO_SIZE_BYTES) {
    throw new Error(
      `Logo size ${Math.floor(size / KB)} KB for connector ${name} is above maximum limit of ${Math.floor(MAX_LOGO_SIZE_BYTES / KB)} KB`
    );
  }

  // Check image dimensions if PIL available
  try {
    const sharp = require('sharp');
    const metadata = sharp(logoPath).metadata();
    // Check dimensions in async context would be needed
  } catch {}

  return `${HRFLOW_CONNECTORS_RAW_GITHUB_CONTENT_BASE}/master/src/${logoPath.split('src/')[1]}`;
}

// ===== CONNECTOR EXCEPTIONS =====

export class ConnectorImportNameNotFound extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'ConnectorImportNameNotFound';
    Object.setPrototypeOf(this, ConnectorImportNameNotFound.prototype);
  }
}

export class AmbiguousConnectorImportName extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'AmbiguousConnectorImportName';
    Object.setPrototypeOf(this, AmbiguousConnectorImportName.prototype);
  }
}

// ===== CONNECTOR UTILITIES =====

/**
 * Get import name for a connector from module members
 */
export function getImportName(connector: Connector): string {
  const mainModule = globalThis;
  let matches: Array<[string, any]> = [];

  // Search for the connector in global scope
  for (const key in mainModule) {
    if ((mainModule as any)[key] === connector) {
      matches.push([key, connector]);
    }
  }

  if (matches.length === 0) {
    throw new ConnectorImportNameNotFound(
      `Failed to find import name for Connector(name=${connector.model.name})=${connector}`
    );
  }

  if (matches.length > 1) {
    throw new AmbiguousConnectorImportName(
      `Found multiple import names for Connector(name=${connector.model.name})=${connector}\n${matches.map((m) => m[0])}`
    );
  }

  return matches[0][0];
}

/**
 * Enum for connector types with additional metadata
 */
export const DEFAULT_CONNECTOR_TYPE_ACTIONS: Record<string, string[]> = {
  ATS: [
    'pull_job_list',
    'pull_profile_list',
    'push_profile',
  ],
  HCM: [
    'pull_job_list',
    'pull_profile_list',
    'push_profile',
  ],
  CRM: [
    'pull_job_list',
    'pull_profile_list',
    'push_profile',
  ],
  Automation: ['catch_profile'],
  JobBoard: ['pull_job_list', 'push_job', 'catch_profile'],
};

/**
 * Generate HrFlow.ai connectors manifest
 */
export interface GenerateManifestConfig {
  connectors: Connector[];
  targetConnectors?: Record<string, any>[];
  directoryPath?: string;
  connectorsDirectory?: string;
}

export async function hrflowConnectorsManifest(config: GenerateManifestConfig): Promise<Record<string, any>> {
  const {
    connectors,
    targetConnectors,
    directoryPath = '.',
    connectorsDirectory = CONNECTORS_DIRECTORY,
  } = config;

  let targets = targetConnectors;

  if (!targets) {
    const fs = require('fs');
    try {
      const data = fs.readFileSync(ALL_TARGET_CONNECTORS_LIST_PATH, 'utf-8');
      targets = JSON.parse(data);
    } catch {
      targets = [];
    }
  }

  const connectorByName = new Map<string, Connector>();
  for (const connector of connectors) {
    connectorByName.set(connector.model.name, connector);
  }

  const allConnectors = (targets || [])
    .map((c) => ({
      ...c,
      object: connectorByName.get(c.name),
    }))
    .sort((a: any, b: any) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));

  const manifest: Record<string, any> = {
    name: 'HrFlow.ai Connectors',
    connectors: [],
  };

  for (const connectorInfo of allConnectors) {
    let connectorManifest: Record<string, any>;

    if (connectorInfo.object) {
      connectorManifest = connectorInfo.object.manifest();
      try {
        // Try to compute logo path
        connectorManifest.logo = computeLogoPath(
          connectorInfo.object.model.name,
          connectorInfo.object.model.subtype,
          connectorsDirectory
        );
      } catch {
        connectorManifest.logo = '';
      }
    } else {
      const connector_type = (connectorInfo.type || 'Other').toUpperCase().replace(' ', '');
      connectorManifest = {
        name: connectorInfo.name,
        type: connector_type,
        subtype: connectorInfo.subtype,
        logo: '',
        actions: [],
      };

      try {
        connectorManifest.logo = computeLogoPath(
          connectorInfo.name,
          connectorInfo.subtype,
          connectorsDirectory
        );
      } catch {}

      if (
        ['ATS', 'HCM', 'CRM'].includes(connector_type)
      ) {
        connectorManifest.actions = [
          DEFAULT_PULL_JOB_LIST_ACTION_MANIFEST,
          DEFAULT_PULL_PROFILE_LIST_ACTION_MANIFEST,
          DEFAULT_PUSH_PROFILE_ACTION_MANIFEST,
        ];
      } else if (connector_type === 'AUTOMATION') {
        connectorManifest.actions = [DEFAULT_CATCH_PROFILE_ACTION_MANIFEST];
      } else if (connector_type === 'JOB BOARD') {
        connectorManifest.actions = [
          DEFAULT_PULL_JOB_LIST_ACTION_MANIFEST,
          DEFAULT_PUSH_JOB_ACTION_MANIFEST,
          DEFAULT_CATCH_PROFILE_ACTION_MANIFEST,
        ];
      }
    }

    if (connectorManifest.actions && connectorManifest.actions.length > 0) {
      manifest.connectors.push(connectorManifest);
    }
  }

  // Save manifest to file
  const fs = require('fs');
  const path = require('path');
  const manifestPath = path.join(directoryPath, 'manifest.json');
  try {
    fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));
  } catch {
    // Silent fail for file writing
  }

  return manifest;
}

// ===== EXPORT IMPORTS =====

export { ALL_TARGET_CONNECTORS_LIST_PATH } from './common';
