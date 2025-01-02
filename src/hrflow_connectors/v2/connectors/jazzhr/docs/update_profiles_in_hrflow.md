# Update profiles in hrflow
`JazzHR` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ JazzHR _to_ HrFlow



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
| `name`  | `string\|null` | None | Any substring in first or last name |
| `city`  | `string\|null` | None |  |
| `job_id`  | `string\|null` | None |  |
| `job_title`  | `string\|null` | None |  |
| `recruiter_id`  | `string\|null` | None |  |
| `apply_date`  | `string\|null` | None | Exact applied date in YYYY-MM-DD format |
| `from_apply_date`  | `string\|null` | None | YYYY-MM-DD |
| `to_apply_date`  | `string\|null` | None | YYYY-MM-DD |
| `status`  | `string\|null` | None | Applicant's workflow status ID |
| `rating`  | `string\|null` | None | Applicant's rating (1-5) |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `string` | None | HrFlow.ai source key |
| `only_edit_fields`  | `array\|null` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

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


JazzHR.update_profiles_in_hrflow(
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
        name=...,
        city=...,
        job_id=...,
        job_title=...,
        recruiter_id=...,
        apply_date=...,
        from_apply_date=...,
        to_apply_date=...,
        status=...,
        rating=...,
    ),
    push_parameters=dict(
        source_key=...,
        only_edit_fields=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```