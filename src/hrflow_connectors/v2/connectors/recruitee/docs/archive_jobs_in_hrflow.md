# Archive jobs in hrflow
`Recruitee` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ Recruitee _to_ HrFlow



## Recruitee Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `string` | None | Company ID. A company subdomain can also be used. |
| `api_token` :red_circle: | `string` | None | Personal API Token allowing access to the Recruitee API from external services. |
| `recruitee_endpoint`  | `Literal['https://api.rc.recruitee.dev/c','https://api.recruitee.com/c','https://api.s.recruitee.com/c']` | https://api.recruitee.com/c | Specifies which endpoint to be used, satging or production. |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Recruitee)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `kind`  | `Literal['job','talent_pool']\|null` | job | If no kind is given, returns all job offers, if kind is job then lists only jobs, if scope is talent_pool, lists only talent pools |
| `scope`  | `string\|null` | active | If no scope is given list all job offers. archived returns only archived job offers, active returns published, internal and closed job offers, not_archived returns all but archived jobs |
| `view_mode`  | `Literal['brief','default']` | default | default (default mode, includes most of offer details); brief (only offer’s id, title, status and kind) |

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
from hrflow_connectors.v2 import Recruitee


logging.basicConfig(level=logging.INFO)


Recruitee.archive_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        company_id=...,
        api_token=...,
        recruitee_endpoint=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        kind=...,
        scope=...,
        view_mode=...,
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