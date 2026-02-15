# HrFlow.ai Connectors - TypeScript/Node.js Edition

Complete TypeScript/Node.js translation of the Python HrFlow.ai Connectors library.

## Features

- 🔄 Full translation of 148+ Python connectors to TypeScript
- 📦 Production-ready code with proper typing
- 🚀 Async/await patterns for modern Node.js
- 🛡️ Type-safe connector definitions
- 📊 Support for profiles, jobs, applications, and more
- 🔌 Warehouse abstraction pattern
- 🔐 Authentication handling for each platform

## Installation

```bash
npm install hrflow-connectors-js
```

## Quick Start

```typescript
import { Bullhorn } from 'hrflow-connectors-js/bullhorn';
import pino from 'pino';

const logger = pino();
const connector = new Bullhorn();

// Pull jobs from Bullhorn
const results = await connector.pull_job_list(
  'workflow-123',
  {
    logics: [],
    format: (item) => item,
    read_mode: 'sync',
  },
  {
    clientId: 'xxx',
    clientSecret: 'xxx',
    username: 'xxx',
    password: 'xxx',
    lastModifiedDate: '0',
    fields: '*',
    query: 'isDeleted:0',
    count: 100,
  },
  {
    // target parameters
  },
);

console.log(results);
```

## Connectors Included

Currently translating 148 connectors:

### ATS Systems
- Bullhorn ✅
- Greenhouse (in progress)
- Ashby (in progress)
- BambooHR
- iCIMS
- Workday
- Oracle Recruiting
- ... and 140+ more

### Job Boards
- Indeed
- LinkedIn
- Glassdoor
- Monster
- CareerBuilder
- ... and more

### CRM Systems
- Salesforce
- HubSpot
- Pipedrive
- ... and more

### HCM Systems
- SAP SuccessFactors
- Workday
- ADP
- ... and more

## Architecture

```
hrflow-connectors-js/
├── v1/
│   ├── core/              # Core framework
│   │   ├── types.ts       # Type definitions
│   │   ├── warehouse.ts   # Warehouse abstraction
│   │   └── connector.ts   # Connector base class
│   └── connectors/        # 148+ connectors
│       ├── bullhorn/
│       ├── greenhouse/
│       ├── ashby/
│       └── ...
└── scripts/
    └── generateConnectors.ts  # Connector stub generation
```

## Core Concepts

### Warehouses

A `Warehouse` represents a data source or sink:

```typescript
interface Warehouse {
  name: string;
  dataType: DataType;  // 'profile' | 'job' | 'other'
  read?: ReadAction;
  write?: WriteAction;
}
```

### Connectors

A `Connector` orchestrates reading from an origin warehouse and writing to a target:

```typescript
const connector = new Connector(
  name: 'Bullhorn',
  type: ConnectorType.ATS,
  subtype: 'bullhorn',
  description: '...',
  url: '...',
  actions: [...]
);
```

### Actions

Each connector has multiple actions (pull_job_list, push_profile, etc):

```typescript
const action = new ConnectorAction({
  name: ActionName.PULL_JOB_LIST,
  triggerType: WorkflowType.PULL,
  origin: BullhornJobWarehouse,
  target: HrFlowJobWarehouse,
  // ... etc
});
```

## Usage Patterns

### Pull Pattern (Inbound)

```typescript
// Read from external system, write to HrFlow
const result = await connector.pull_profile_list(
  workflowId,
  actionParameters,
  originParameters,
  targetParameters,
);
```

### Push Pattern (Outbound)

```typescript
// Read from HrFlow, write to external system
const result = await connector.push_profile(
  workflowId,
  actionParameters,
  originParameters,
  targetParameters,
);
```

### Catch/Hook Pattern

```typescript
// Listen for webhooks, transform, and push
const result = await connector.catch_profile(
  workflowId,
  actionParameters,
  originParameters,
  targetParameters,
);
```

## Action Parameters

### Common Parameters

All actions support:

```typescript
interface ActionParameters {
  logics?: LogicFunction[];      // Filter/transform items
  format?: FormatFunction;        // Transform item structure
  event_parser?: EventParserFunction;  // Parse webhook events
  read_mode?: ReadMode;           // 'sync' | 'incremental'
}
```

### Logic Functions

```typescript
type LogicFunction = (item: Record<string, any>) => Record<string, any> | null;

const logics = [
  (item) => {
    // Filter: return null to discard
    if (!item.email) return null;
    return item;
  },
  (item) => {
    // Transform: modify and return
    return { ...item, processed: true };
  },
];
```

### Format Functions

```typescript
type FormatFunction = (item: Record<string, any>) => Record<string, any>;

const format = (item) => {
  return {
    id: item.candidateId,
    name: `${item.firstName} ${item.lastName}`,
    email: item.email,
  };
};
```

## Advanced Features

### Incremental Reads

Track last read position to only fetch new/modified items:

```typescript
const result = await connector.pull_profile_list(
  workflowId,
  { read_mode: 'incremental' },
  // ... parameters
);

// Next run uses result.readFrom
const nextResult = await connector.pull_profile_list(
  workflowId,
  { read_mode: 'incremental' },
  { ...parameters, lastModifiedDate: result.readFrom },
  // ... parameters
);
```

### Error Handling

Results include detailed error tracking:

```typescript
interface RunResult {
  status: Status;        // 'success' | 'success_with_failures' | 'fatal'
  reason: Reason;        // Specific failure reason
  events: EventCounter;  // Count of each event type
  readFrom?: string;     // For incremental reads
}

// Example
if (result.status === Status.FATAL) {
  console.error(`Failed with reason: ${result.reason}`);
  console.error(`Read failures: ${result.events[Event.READ_FAILURE]}`);
}
```

## Migration from Python

If migrating from Python version:

```python
# Python
from hrflow_connectors import Bullhorn
connector = Bullhorn()
result = connector.pull_job_list(...)
```

```typescript
// TypeScript
import { Bullhorn } from 'hrflow-connectors-js/bullhorn';
const connector = new Bullhorn();
const result = await connector.pull_job_list(...);
```

Main differences:
1. Add `import` statements for each connector
2. Use `await` for async operations
3. Use `async function*` instead of generators
4. Type annotations for parameters

## Testing

Run tests:

```bash
npm test
```

Example test:

```typescript
import { Bullhorn } from 'hrflow-connectors-js/bullhorn';

describe('Bullhorn', () => {
  it('should read profiles', async () => {
    const connector = new Bullhorn();
    const result = await connector.pull_profile_list(
      'test-workflow',
      { logics: [], format: (x) => x },
      { /* auth params */ },
      { /* target params */ },
    );

    expect(result.status).toBe(Status.SUCCESS);
  });
});
```

## Contributing

Contributions welcome! To add a new connector:

1. Copy connector pattern from Bullhorn
2. Translate Python files to TypeScript
3. Add tests
4. Update documentation
5. Submit PR

See [TRANSLATION_GUIDE.md](./TRANSLATION_GUIDE.md) for details.

## API Documentation

Full API documentation available in [docs/](./docs/) directory.

## License

[Insert license here]

## Status

This is an ongoing translation project. Currently implemented:
- ✅ Core framework
- ✅ Bullhorn connector (fully featured example)
- 🟡 147 additional connectors (in progress)

Expected completion: [Date TBD based on contribution velocity]

## Support

- GitHub Issues: [Open an issue](https://github.com/Riminder/hrflow-connectors-js/issues)
- Documentation: [Link to docs](./TRANSLATION_GUIDE.md)
- Examples: [Link to examples](./examples/)
