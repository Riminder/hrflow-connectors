/**
 * Warehouse module - Core warehouse definitions and actions
 * TypeScript translation of warehouse.py (236 lines)
 */

/**
 * Custom error classes for warehouse operations
 */

export class FieldNotFoundError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'FieldNotFoundError';
    Object.setPrototypeOf(this, FieldNotFoundError.prototype);
  }
}

export class FixedValueValidationError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'FixedValueValidationError';
    Object.setPrototypeOf(this, FixedValueValidationError.prototype);
  }
}

export class InvalidFieldError extends TypeError {
  constructor(message: string) {
    super(message);
    this.name = 'InvalidFieldError';
    Object.setPrototypeOf(this, InvalidFieldError.prototype);
  }
}

export class NoFieldTypeError extends TypeError {
  constructor(message: string) {
    super(message);
    this.name = 'NoFieldTypeError';
    Object.setPrototypeOf(this, NoFieldTypeError.prototype);
  }
}

export class BadFieldTypeError extends TypeError {
  constructor(message: string) {
    super(message);
    this.name = 'BadFieldTypeError';
    Object.setPrototypeOf(this, BadFieldTypeError.prototype);
  }
}

/**
 * Enum for data types
 */
export enum DataType {
  Profile = 'profile',
  Job = 'job',
  Other = 'other',
}

/**
 * Enum for action types
 */
export enum ActionType {
  read = 'read',
  write = 'write',
}

/**
 * Enum for read modes
 */
export enum ReadMode {
  sync = 'sync',
  incremental = 'incremental',
}

/**
 * Enum for field types in parameters
 */
export enum FieldType {
  Auth = 'Auth',
  QueryParam = 'Query Param',
  Other = 'Other',
}

/**
 * Represents an endpoint with name, description, and URL
 */
export interface ActionEndpoints {
  name: string;
  description: string;
  url: string;
}

/**
 * Field type example constant
 */
const FIELD_TYPE_EXAMPLE = `
    Example :
        from pydantic import Field

        from hrflow_connectors.v1.core import FieldType

        class MyParams(ParametersModel):
            my_field: str = Field(
                ..., description="My field", field_type=FieldType.Other
            )
`;

/**
 * Error messages for field validation
 */
const INVALID_FIELD_ERROR_MSG = `Field '{}' in {} should have proper annotation using pydantic.Field.
    ${FIELD_TYPE_EXAMPLE}`;

const NO_FIELD_TYPE_ERROR_MSG = `Field '{}' in {} is missing 'field_type' declaration.
    ${FIELD_TYPE_EXAMPLE}`;

const BAD_FIELD_TYPE_ERROR_MSG = `'field_type' for field '{}' in {} should be defined using
    \`hrflow_connectors.core.FieldType\`.
    ${FIELD_TYPE_EXAMPLE}`;

/**
 * Metadata for a parameter field
 */
export interface FieldMetadata {
  type: string;
  description?: string;
  fieldType?: FieldType;
  required?: boolean;
  default?: any;
  const?: boolean;
  [key: string]: any;
}

/**
 * Base class for parameters that enforces field requirements
 */
export class ParametersModel {
  protected static fieldsMetadata: Map<string, FieldMetadata> = new Map();

  constructor(data: Record<string, any> = {}) {
    this.validate(data);
    Object.assign(this, data);
  }

  protected validate(data: Record<string, any>): void {
    const fieldsMetadata = (this.constructor as typeof ParametersModel).fieldsMetadata;

    // Validate field types and required fields
    for (const [fieldName, fieldMeta] of fieldsMetadata) {
      if (fieldMeta.required && data[fieldName] === undefined && !fieldMeta.const) {
        throw new InvalidFieldError(
          INVALID_FIELD_ERROR_MSG.replace('{}', fieldName).replace('{}', this.constructor.name)
        );
      }
      if (fieldMeta.fieldType === undefined && !fieldMeta.const) {
        throw new NoFieldTypeError(
          NO_FIELD_TYPE_ERROR_MSG.replace('{}', fieldName).replace('{}', this.constructor.name)
        );
      }
      if (
        fieldMeta.fieldType &&
        !Object.values(FieldType).includes(fieldMeta.fieldType)
      ) {
        throw new BadFieldTypeError(
          BAD_FIELD_TYPE_ERROR_MSG.replace('{}', fieldName).replace('{}', this.constructor.name)
        );
      }
    }

    // Check for extra fields (forbid extra)
    for (const key of Object.keys(data)) {
      if (!fieldsMetadata.has(key)) {
        throw new Error(`Extra field '${key}' not allowed`);
      }
    }
  }

  /**
   * Get JSON schema representation of this model
   */
  schema(): Record<string, any> {
    const fieldsMetadata = (this.constructor as typeof ParametersModel).fieldsMetadata;
    const properties: Record<string, any> = {};
    const required: string[] = [];

    for (const [fieldName, fieldMeta] of fieldsMetadata) {
      if (!fieldMeta.const) {
        properties[fieldName] = {
          type: fieldMeta.type,
          description: fieldMeta.description || '',
        };
        if (fieldMeta.required) {
          required.push(fieldName);
        }
      }
    }

    return {
      type: 'object',
      properties,
      required,
      additionalProperties: false,
    };
  }
}

/**
 * Type definition for warehouse read function
 */
export type WarehouseReadFunction = (
  adapter: any,
  parameters: ParametersModel,
  readMode?: ReadMode,
  readFrom?: string
) => AsyncIterable<Record<string, any>>;

/**
 * Type definition for warehouse write function
 */
export type WarehouseWriteFunction = (
  adapter: any,
  parameters: ParametersModel,
  items: Record<string, any>[]
) => Promise<Record<string, any>[]>;

/**
 * Type definition for item-to-read-from function
 */
export type ItemToReadFromFunction = (item: Record<string, any>) => string;

/**
 * Configuration for warehouse read action
 */
export interface WarehouseReadActionConfig {
  endpoints?: ActionEndpoints[];
  parameters: typeof ParametersModel;
  function: WarehouseReadFunction;
  itemToReadFrom?: ItemToReadFromFunction;
  supportsIncremental?: boolean;
}

/**
 * Configuration for warehouse write action
 */
export interface WarehouseWriteActionConfig {
  endpoints?: ActionEndpoints[];
  parameters: typeof ParametersModel;
  function: WarehouseWriteFunction;
}

/**
 * Warehouse read action implementation
 */
export class WarehouseReadAction {
  endpoints: ActionEndpoints[];
  parameters: typeof ParametersModel;
  function: WarehouseReadFunction;
  itemToReadFrom?: ItemToReadFromFunction;
  supportsIncremental: boolean;

  constructor(config: WarehouseReadActionConfig) {
    this.endpoints = config.endpoints || [];
    this.parameters = config.parameters;
    this.function = config.function;
    this.itemToReadFrom = config.itemToReadFrom;
    this.supportsIncremental = config.supportsIncremental || false;

    // Validate that incremental mode has itemToReadFrom function
    if (this.supportsIncremental && !this.itemToReadFrom) {
      throw new Error(
        'Function itemToReadFrom must be provided when supportsIncremental is True'
      );
    }
  }

  /**
   * Execute the read function
   */
  async *call(
    adapter: any,
    parameters: ParametersModel,
    readMode?: ReadMode,
    readFrom?: string
  ): AsyncIterable<Record<string, any>> {
    yield* this.function(adapter, parameters, readMode, readFrom);
  }
}

/**
 * Warehouse write action implementation
 */
export class WarehouseWriteAction {
  endpoints: ActionEndpoints[];
  parameters: typeof ParametersModel;
  function: WarehouseWriteFunction;

  constructor(config: WarehouseWriteActionConfig) {
    this.endpoints = config.endpoints || [];
    this.parameters = config.parameters;
    this.function = config.function;
  }

  /**
   * Execute the write function
   */
  async call(
    adapter: any,
    parameters: ParametersModel,
    items: Record<string, any>[]
  ): Promise<Record<string, any>[]> {
    return this.function(adapter, parameters, items);
  }
}

/**
 * Warehouse configuration
 */
export interface WarehouseConfig {
  name: string;
  dataType: DataType;
  dataSchema?: typeof ParametersModel;
  read?: WarehouseReadActionConfig;
  write?: WarehouseWriteActionConfig;
}

/**
 * Warehouse represents a data source or sink
 */
export class Warehouse {
  name: string;
  dataType: DataType;
  dataSchema?: Record<string, any>;
  read?: WarehouseReadAction;
  write?: WarehouseWriteAction;

  constructor(config: WarehouseConfig) {
    this.name = config.name;
    this.dataType = config.dataType;
    this.dataSchema = config.dataSchema ? (config.dataSchema as any).schema?.() : undefined;

    if (config.read) {
      this.read = new WarehouseReadAction(config.read);
    }
    if (config.write) {
      this.write = new WarehouseWriteAction(config.write);
    }
  }

  /**
   * Check if warehouse is readable
   */
  get isReadable(): boolean {
    return this.read !== undefined;
  }

  /**
   * Check if warehouse is writable
   */
  get isWritable(): boolean {
    return this.write !== undefined;
  }

  /**
   * Check if warehouse supports incremental read mode
   */
  get supportsIncremental(): boolean {
    return this.read?.supportsIncremental || false;
  }

  /**
   * Extract read_from value from an item
   */
  itemToReadFrom(item: Record<string, any>): string {
    if (!this.read?.itemToReadFrom) {
      throw new Error('itemToReadFrom is not defined for this warehouse');
    }
    return this.read.itemToReadFrom(item);
  }

  /**
   * Create a new warehouse with fixed read parameters
   */
  withFixedReadParameters(toFix: Record<string, any>): Warehouse {
    return this.withFixedParameters(ActionType.read, toFix);
  }

  /**
   * Create a new warehouse with fixed write parameters
   */
  withFixedWriteParameters(toFix: Record<string, any>): Warehouse {
    return this.withFixedParameters(ActionType.write, toFix);
  }

  /**
   * Internal method to implement fixed parameters logic
   */
  private withFixedParameters(actionType: ActionType, toFix: Record<string, any>): Warehouse {
    const action = actionType === ActionType.read ? this.read : this.write;

    if (!action) {
      throw new Error(`Warehouse does not have ${actionType} action`);
    }

    const originalFields = action.parameters.fieldsMetadata || new Map();

    // Validate that all fields to fix exist
    for (const field of Object.keys(toFix)) {
      if (!originalFields.has(field)) {
        throw new FieldNotFoundError(
          `The field you are trying to fix '${field}' is not part of the available parameters ${Array.from(originalFields.keys())}`
        );
      }

      // Validate the fixed value
      try {
        new action.parameters({ [field]: toFix[field] });
      } catch (e) {
        throw new FixedValueValidationError(
          `The value='${toFix[field]}' you are trying to use for field='${field}' does not pass the original validation with error=${e}`
        );
      }
    }

    // Create config for new warehouse
    const config: WarehouseConfig = {
      name: this.name,
      dataType: this.dataType,
      dataSchema: undefined,
    };

    if (actionType === ActionType.read && this.read) {
      config.read = {
        endpoints: this.read.endpoints,
        parameters: this.createFixedParametersModel(action.parameters, toFix),
        function: this.read.function,
        itemToReadFrom: this.read.itemToReadFrom,
        supportsIncremental: this.read.supportsIncremental,
      };
      config.write = this.write
        ? {
            endpoints: this.write.endpoints,
            parameters: this.write.parameters,
            function: this.write.function,
          }
        : undefined;
    } else if (actionType === ActionType.write && this.write) {
      config.read = this.read
        ? {
            endpoints: this.read.endpoints,
            parameters: this.read.parameters,
            function: this.read.function,
            itemToReadFrom: this.read.itemToReadFrom,
            supportsIncremental: this.read.supportsIncremental,
          }
        : undefined;
      config.write = {
        endpoints: this.write.endpoints,
        parameters: this.createFixedParametersModel(action.parameters, toFix),
        function: this.write.function,
      };
    }

    return new Warehouse(config);
  }

  /**
   * Create a new parameters model class with fixed values
   */
  private createFixedParametersModel(
    baseClass: typeof ParametersModel,
    fixedValues: Record<string, any>
  ): typeof ParametersModel {
    class FixedParametersModel extends baseClass {
      constructor(data: Record<string, any> = {}) {
        const merged = { ...fixedValues, ...data };
        super(merged);
      }
    }
    return FixedParametersModel;
  }
}

export default Warehouse;
