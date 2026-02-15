/**
 * HrFlow.ai Connectors Core Types
 * TypeScript translation of core Python types
 */

import {  Logger } from 'pino';

/** Enum for different data types supported by warehouses */
export enum DataType {
  PROFILE = 'profile',
  JOB = 'job',
  OTHER = 'other',
}

/** Enum for action types (read/write) */
export enum ActionType {
  READ = 'read',
  WRITE = 'write',
}

/** Enum for read modes */
export enum ReadMode {
  SYNC = 'sync',
  INCREMENTAL = 'incremental',
}

/** Enum for field types in parameters */
export enum FieldType {
  AUTH = 'Auth',
  QUERY_PARAM = 'Query Param',
  OTHER = 'Other',
}

/** Action events that can occur during connector execution */
export enum Event {
  READ_SUCCESS = 'read_success',
  READ_FAILURE = 'read_failure',
  FORMAT_FAILURE = 'format_failure',
  LOGICS_DISCARD = 'logics_discard',
  LOGICS_FAILURE = 'logics_failure',
  WRITE_FAILURE = 'write_failure',
  CALLBACK_FAILURE = 'callback_failure',
  CALLBACK_EXECUTED = 'callback_executed',
  ITEM_TO_READ_FROM_FAILURE = 'item_to_read_from_failure',
}

/** Execution status of an action */
export enum Status {
  SUCCESS = 'success',
  SUCCESS_WITH_FAILURES = 'success_with_failures',
  FATAL = 'fatal',
}

/** Reasons for action failures */
export enum Reason {
  ITEM_TO_READ_FROM_FAILURE = 'item_to_read_from_failure',
  ORIGIN_DOES_NOT_SUPPORT_INCREMENTAL = 'origin_does_not_support_incremental',
  BACKEND_NOT_CONFIGURED_IN_INCREMENTAL_MODE = 'backend_not_configured_in_incremental_mode',
  WORKFLOW_ID_NOT_FOUND = 'workflow_id_not_found',
  EVENT_PARSING_FAILURE = 'event_parsing_failure',
  BAD_ACTION_PARAMETERS = 'bad_action_parameters',
  BAD_ORIGIN_PARAMETERS = 'bad_origin_parameters',
  BAD_TARGET_PARAMETERS = 'bad_target_parameters',
  FORMAT_FAILURE = 'format_failure',
  LOGICS_FAILURE = 'logics_failure',
  READ_FAILURE = 'read_failure',
  WRITE_FAILURE = 'write_failure',
  NONE = '',
}

/** Connector types */
export enum ConnectorType {
  ATS = 'ATS',
  CRM = 'CRM',
  HCM = 'HCM',
  AUTOMATION = 'Automation',
  JOB_BOARD = 'Job Board',
  CLASSIFIEDS = 'Classified Ads',
  OTHER = 'Other',
}

/** Action names supported */
export enum ActionName {
  PULL_APPLICATION_LIST = 'pull_application_list',
  PULL_JOB_LIST = 'pull_job_list',
  PULL_PROFILE_LIST = 'pull_profile_list',
  PULL_RESUME_ATTACHMENT_LIST = 'pull_resume_attachment_list',
  PUSH_PROFILE = 'push_profile',
  PUSH_JOB = 'push_job',
  PUSH_PROFILE_LIST = 'push_profile_list',
  PUSH_JOB_LIST = 'push_job_list',
  PUSH_SCORE_LIST = 'push_score_list',
  CATCH_PROFILE = 'catch_profile',
  CATCH_JOB = 'catch_job',
  PUSH_APPLICATION = 'push_application',
  APPLICANT_NEW = 'applicant_new',
  APPLICANT_RESUME_UPDATE = 'applicant_resume_update',
  APPLICANT_UPDATE = 'applicant_update',
}

/** Workflow types (trigger types) */
export enum WorkflowType {
  CATCH = 'hook',
  PULL = 'schedule',
}

/** Type for logic functions that process items */
export type LogicFunction = (item: Record<string, any>) => Record<string, any> | null;

/** Type for format functions */
export type FormatFunction = (item: Record<string, any>) => Record<string, any>;

/** Type for event parser functions */
export type EventParserFunction = (event: Record<string, any>) => Record<string, any>;

/** Type for callbacks executed after action completion */
export type CallbackFunction = (
  originParams: Record<string, any>,
  targetParams: Record<string, any>,
  events: Map<Event, number>,
  items: Record<string, any>[],
) => void;

/** Type for warehouse read functions */
export type WarehouseReadFunction = (
  logger: Logger,
  parameters: Record<string, any>,
  readMode?: ReadMode,
  readFrom?: string,
) => AsyncIterable<Record<string, any>>;

/** Type for warehouse write functions */
export type WarehouseWriteFunction = (
  logger: Logger,
  parameters: Record<string, any>,
  items: Record<string, any>[],
) => Promise<Record<string, any>[]>;

/** Action endpoints information */
export interface ActionEndpoint {
  name: string;
  description: string;
  url: string;
}

/** Parameters for a warehouse action */
export interface ActionParameters {
  logics?: LogicFunction[];
  format?: FormatFunction;
  event_parser?: EventParserFunction;
  read_mode?: ReadMode;
}

/** Field information with validation */
export interface FieldInfo {
  type: string;
  description?: string;
  fieldType: FieldType;
  required?: boolean;
  default?: any;
}

/** Base parameters model for actions */
export interface BaseActionParameters {
  logics: LogicFunction[];
  format: FormatFunction;
  event_parser?: EventParserFunction;
  read_mode: ReadMode;
}

/** Warehouse read action definition */
export interface ReadActionDefinition {
  endpoints: ActionEndpoint[];
  parameters: new(...args: any[]) => any;
  function: WarehouseReadFunction;
  itemToReadFrom?: (item: Record<string, any>) => string;
  supportsIncremental: boolean;
}

/** Warehouse write action definition */
export interface WriteActionDefinition {
  endpoints: ActionEndpoint[];
  parameters: new(...args: any[]) => any;
  function: WarehouseWriteFunction;
}

/** Counter for tracking events */
export interface EventCounter {
  [Event.READ_SUCCESS]: number;
  [Event.READ_FAILURE]: number;
  [Event.FORMAT_FAILURE]: number;
  [Event.LOGICS_DISCARD]: number;
  [Event.LOGICS_FAILURE]: number;
  [Event.WRITE_FAILURE]: number;
  [Event.CALLBACK_FAILURE]: number;
  [Event.CALLBACK_EXECUTED]: number;
  [Event.ITEM_TO_READ_FROM_FAILURE]: number;
}

/** Result of an action run */
export interface RunResult {
  status: Status;
  reason: Reason;
  events: EventCounter;
  readFrom?: string;
}

/** Error details during action initialization */
export interface ActionInitError {
  data: Record<string, any>;
  reason: Reason;
}

/** Warehouse schema definition */
export interface WarehouseSchema {
  name: string;
  dataType: DataType;
  dataSchema?: Record<string, any>;
  read?: ReadActionDefinition;
  write?: WriteActionDefinition;
  supportsIncremental?: boolean;
}

/** Connector action definition */
export interface ConnectorActionDefinition {
  name: ActionName;
  triggerType: WorkflowType;
  description: string;
  parameters: new(...args: any[]) => BaseActionParameters;
  origin: Warehouse;
  target: Warehouse;
  actionType: 'inbound' | 'outbound';
  callback?: CallbackFunction;
}

/** Manifest for a connector action */
export interface ActionManifest {
  name: string;
  action_type: string;
  action_parameters: Record<string, any>;
  data_type: string;
  trigger_type: string;
  origin: string;
  origin_parameters: Record<string, any>;
  origin_data_schema: Record<string, any>;
  supports_incremental: boolean;
  target: string;
  target_parameters: Record<string, any>;
  target_data_schema: Record<string, any>;
  jsonmap: Record<string, any>;
  workflow_code: string;
  workflow_code_format_placeholder: string;
  workflow_code_logics_placeholder: string;
  workflow_code_event_parser_placeholder?: string;
  workflow_code_workflow_id_settings_key: string;
  workflow_code_origin_settings_prefix: string;
  workflow_code_target_settings_prefix: string;
}

/** Manifest for a connector */
export interface ConnectorManifest {
  name: string;
  type: string;
  subtype: string;
  logo: string;
  actions: ActionManifest[];
}

/** Global HrFlow.ai connectors manifest */
export interface HrFlowConnectorsManifest {
  name: string;
  connectors: ConnectorManifest[];
}
