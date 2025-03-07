# Create profiles in recruitee
`HrFlow` :arrow_right: `Recruitee`

Send **created** 'profile(s)' _from_ HrFlow _to_ Recruitee



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

## Pull Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `string` | None | HrFlow.ai source key |
| `profile_key` :red_circle: | `string` | None | HrFlow.ai profile key |

## Push Parameters (Recruitee)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `offers`  | `array\|null` | None | Offers to which the candidate will be assigned with default stage. |

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


Recruitee.create_profiles_in_recruitee(
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
        source_key=...,
        profile_key=...,
    ),
    push_parameters=dict(
        offers=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```