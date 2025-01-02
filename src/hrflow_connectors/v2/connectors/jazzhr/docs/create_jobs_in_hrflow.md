# Create jobs in hrflow
`JazzHR` :arrow_right: `HrFlow`

Send **created** 'job(s)' _from_ JazzHR _to_ HrFlow



## JazzHR Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_key` :red_circle: | `string` | None | The API key to authenticate with JazzHR |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (JazzHR)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `title`  | `string\|null` | None | Title of the Job |
| `recruiter`  | `string\|null` | None | Recruiter of the Job |
| `board_code`  | `string\|null` | None | Board Code of the Job |
| `department`  | `string\|null` | None | Department of the Job |
| `hiring_lead`  | `string\|null` | None | Hiring Lead of the Job |
| `state`  | `string\|null` | None | Location of Job by State |
| `city`  | `string\|null` | None | Location of Job by City |
| `from_open_date`  | `string\|null` | None | Filter by job open date. Use YYYY-MM-DD format |
| `to_open_date`  | `string\|null` | None | Filter by job open date. Use YYYY-MM-DD format |
| `status`  | `Literal['Approved','Cancelled','Closed','Drafting','Filled','Needs Approval','On Hold','Open']\|null` | None | Filter by Job Status |
| `confidential`  | `boolean\|null` | None | Filter by confidentiality |
| `private`  | `boolean\|null` | None | Filter by privacy |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |
| `enrich_with_parsing`  | `boolean` | False | When enabled jobs are enriched with HrFlow.ai parsing |

## Other Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `workflow_id` :red_circle: | `string` | None | A stable identifier used for persisting in incremental mode |
| `logics` :red_circle: | `array\|null` | None | A list of functions called in sequence with each item pulled from the origin. Each function might either return it's argument or None to discard the item. Any item discarded is eventually not pushed to the target |
| `format`  | `Callable\|null` | None | A formatting function to apply on items pulled before the push |
| `callback`  | `Callable\|null` | None | Registers a callback function to be called at the of a successful execution |
| `persist`  | `boolean` | True | When False has the effect of running in dry mode. Items are pulled but not pushed to the target |
| `incremental`  | `boolean` | False | Controls the incremental reading execution mode |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors.v2 import JazzHR


logging.basicConfig(level=logging.INFO)


JazzHR.create_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        title=...,
        recruiter=...,
        board_code=...,
        department=...,
        hiring_lead=...,
        state=...,
        city=...,
        from_open_date=...,
        to_open_date=...,
        status=...,
        confidential=...,
        private=...,
    ),
    push_parameters=dict(
        board_key=...,
        enrich_with_parsing=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```