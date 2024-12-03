# Archive jobs in hrflow
`SmartRecruiters` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ SmartRecruiters _to_ HrFlow



## SmartRecruiters Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `string` | None | X-SmartToken used to access SmartRecruiters API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (SmartRecruiters)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `query`  | `string\|null` | None | Case insensitive full-text query against job title e.g. java developer |
| `updated_after`  | `string\|null` | None | ISO8601-formatted time boundaries for the job update time |
| `posting_status`  | `Literal['INTERNAL','NOT_PUBLISHED','PRIVATE','PUBLIC']\|null` | None | Posting status of a job. One of ['PUBLIC', 'INTERNAL', 'NOT_PUBLISHED', 'PRIVATE'] |
| `job_status`  | `Literal['CANCELLED','CREATED','FILLED','INTERVIEW','OFFER','ON_HOLD','SOURCING']\|null` | None | Status of a job. One of ['CREATED', 'SOURCING', 'FILLED', 'INTERVIEW', 'OFFER', 'CANCELLED', 'ON_HOLD'] |
| `limit`  | `integer` | 100 | Number of items to pull from SmartRecruiters at a time. |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |

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
from hrflow_connectors.v2 import SmartRecruiters


logging.basicConfig(level=logging.INFO)


SmartRecruiters.archive_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        x_smart_token=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        query=...,
        updated_after=...,
        posting_status=...,
        job_status=...,
        limit=...,
    ),
    push_parameters=dict(
        board_key=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```