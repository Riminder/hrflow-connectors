/**
 * Comprehensive TypeScript Translation Guide
 * Converting 148 Python Connectors to TypeScript/Node.js
 */

# HrFlow.ai Connectors - TypeScript/Node.js Translation Guide

## Overview

This guide documents the complete translation of the Python HrFlow.ai Connectors project (148+ connectors) into TypeScript/Node.js.

## Architecture

### Directory Structure

```
src/hrflow_connectors_js/
├── v1/
│   ├── core/              # Core framework (types, warehouse, connector)
│   │   ├── types.ts       # Type definitions and enums
│   │   ├── warehouse.ts   # Warehouse class and actions
│   │   ├── connector.ts   # Connector and ConnectorAction classes
│   │   └── index.ts       # Core exports
│   ├── connectors/        # All 148+ connectors
│   │   ├── bullhorn/      # Example: Bullhorn connector (fully translated)
│   │   │   ├── index.ts   # Connector definition
│   │   │   ├── schemas.ts # Data models
│   │   │   ├── warehouse.ts # Warehouse implementations
│   │   │   └── utils.ts   # Utility functions
│   │   ├── ashby/         # Additional connectors follow same pattern
│   │   ├── greenhouse/
│   │   └── ... (140+ more connectors)
│   └── index.ts           # Main exports
├── scripts/
│   └── generateConnectors.ts # Script to generate connector stubs
├── package.json           # Dependencies
├── tsconfig.json          # TypeScript configuration
└── README.md              # Project documentation
```

## Key Design Patterns

### 1. Core Types (types.ts)

All enums and interfaces are translated 1-to-1 from Python:

```typescript
export enum DataType {
  PROFILE = 'profile',
  JOB = 'job',
  OTHER = 'other',
}

export enum ActionName {
  PULL_PROFILE_LIST = 'pull_profile_list',
  PUSH_PROFILE = 'push_profile',
  // ... etc
}
```

### 2. Warehouse Pattern (warehouse.ts)

Portuguese: BaseModel becomes ParametersModel, validation happens in constructor:

```typescript
export abstract class ParametersModel {
  constructor(data: Record<string, any>) {
    this.validate(data);
    Object.assign(this, data);
  }

  protected validate(data: Record<string, any>): void {
    // Validation logic
  }

  schema(): Record<string, any> {
    // Return JSON schema
  }
}
```

### 3. Connector Pattern (connector.ts)

```typescript
export class Connector {
  model: {
    name: string;
    type: ConnectorType;
    actions: ConnectorAction[];
  };

  constructor(name, type, subtype, description, url, actions) {
    // Bind actions as methods
  }

  manifest(): ConnectorManifest {
    // Generate manifest
  }
}
```

### 4. Individual Connector Pattern

Each connector follows this structure:

**schemas.ts** - Data type definitions
```typescript
export interface BullhornProfile {
  id?: string;
  name?: string;
  // ... fields
}
```

**warehouse.ts** - Read/Write actions
```typescript
async function* readProfiles(
  logger: Logger,
  parameters: any,
): AsyncIterable<BullhornProfile> {
  // Implementation
}

export const BullhornProfileWarehouse = new Warehouse({
  name: 'Bullhorn Profiles',
  dataType: DataType.PROFILE,
  read: { /* ... */ },
  write: { /* ... */ },
});
```

**index.ts** - Connector definition
```typescript
export const Bullhorn = new Connector(
  'Bullhorn',
  ConnectorType.ATS,
  'bullhorn',
  'Description',
  'https://...',
  [
    new ConnectorAction({
      name: ActionName.PULL_JOB_LIST,
      // ... action definition
    }),
  ],
);
```

## Translation Mapping

### Python to TypeScript Conversions

| Python | TypeScript |
|--------|-----------|
| `BaseModel` | `interface` or `ParametersModel` |
| `Enum` | `enum` |
| `t.List[T]` | `T[]` |
| `t.Dict` | `Record<string, any>` |
| `t.Optional[T]` | `T \| undefined` |
| `t.Union[T1, T2]` | `T1 \| T2` |
| `@property` | `get propertyName()` |
| `generator` | `async function*` |
| `LoggerAdapter` | `pino.Logger` |
| `requests` | `axios` |

### Key Python Features Translated

#### 1. Pydantic Models → Interfaces + Classes

Python:
```python
class BaseParameters(BaseModel):
    name: str = Field(..., description="Name")
    
    class Config:
        extra = "forbid"
```

TypeScript:
```typescript
export class BaseParameters extends ParametersModel {
  name: string;
  
  constructor(data: Record<string, any>) {
    super(data);
    this.name = data.name;
  }
}
```

#### 2. Generators → Async Generators

Python:
```python
def read_jobs(adapter, parameters, read_mode=None, read_from=None):
    while True:
        # fetch data
        yield job
```

TypeScript:
```typescript
async function* readJobs(
  logger: Logger,
  parameters: any,
): AsyncIterable<Job> {
  while (true) {
    // fetch data
    yield job;
  }
}
```

#### 3. Type Hints → TypeScript Types

Python:
```python
def run(
    self,
    connector_name: str,
    items: t.List[t.Dict],
) -> RunResult:
```

TypeScript:
```typescript
async run(
  connectorName: string,
  items: Record<string, any>[],
): Promise<RunResult> {
```

## Connector Translation Process

### Step 1: Read Python Files
- Read `connector.py` - connector definition and format functions
- Read `warehouse.py` - warehouse and action definitions
- Read `schemas.py` - data models
- Read utility files as needed

### Step 2: Create TypeScript Schemas
Create `schemas.ts` with interfaces matching Pydantic models.

### Step 3: Create TypeScript Warehouse
Create `warehouse.ts` with:
- Parameter classes extending `ParametersModel`
- Read/Write functions as async generators/functions
- Warehouse instances

### Step 4: Create TypeScript Connector
Create `index.ts` with:
- Connector instance
- Format functions
- All actions defined

### Step 5: Utilities
Create `utils.ts` for helper functions (auth, formatting, etc).

## Implementation Status

### Completed
- ✅ Core framework (types, warehouse, connector)
- ✅ Bullhorn connector (full example)
- ✅ Connector generator script
- ✅ TypeScript configuration
- ✅ Package management setup

### In Progress
- 🟡 Additional connectors (148 total)

### Generation Strategy

Rather than manually translating all 148 connectors, a two-phase approach is used:

**Phase 1: Template Generation**
- Create `generateConnectors.ts` script
- Generates basic stubs for all 148 connectors
- Auto-creates directory structure

**Phase 2: Manual Implementation**
- Populate each connector based on Python source
- Can be parallelized across multiple developers
- Each follows the established pattern

## Running the Project

### Setup
```bash
cd src/hrflow_connectors_js
npm install
npm run build
```

### Generate Connector Stubs
```bash
npm run generate:connectors
```

### Development
```bash
npm run dev  # Watch mode compilation
```

### Testing
```bash
npm test
```

## Adding a New Connector

1. Create directory: `src/hrflow_connectors_js/v1/connectors/{name}/`
2. Create files:
   - `schemas.ts` - Data models
   - `warehouse.ts` - Warehouse and actions
   - `utils.ts` - Utilities
   - `index.ts` - Connector definition
3. Add to `v1/connectors/index.ts` exports
4. Add to main `v1/index.ts` exports

## Dependencies

- **axios** - HTTP client
- **pino** - Logging
- **uuid** - ID generation
- **typescript** - Language
- **jest** - Testing

## Testing Strategy

Each connector should have:
1. Unit tests for utility functions
2. Integration tests for warehouse actions
3. Validation tests for parameter schemas

Example test structure:
```
tests/v1/connectors/bullhorn/
├── schemas.test.ts
├── warehouse.test.ts
├── connector.test.ts
└── utils.test.ts
```

## Performance Considerations

1. **Async Generators** - Used for large data sets (jobs, profiles)
2. **Streaming** - Items are processed one at a time, not loaded into memory
3. **Error Handling** - Each item failure tracked separately
4. **Caching** - Per-warehouse caching of API responses

## Migration from Python

For users migrating from Python version:

1. Install npm package: `npm install hrflow-connectors-js`
2. Import connectors: `import { Bullhorn } from 'hrflow-connectors-js/bullhorn'`
3. Usage is identical to Python version
4. Async/await used instead of generators

## Contributing

To contribute translations:

1. Fork repository
2. Choose an untranslated connector
3. Follow the established patterns
4. Add tests
5. Submit pull request

## Support

For issues or questions:
- Create GitHub issue
- Reference Python source for comparison
- Include error logs and details
