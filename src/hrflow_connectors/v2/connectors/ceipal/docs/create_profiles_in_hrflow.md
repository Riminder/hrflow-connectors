# Create profiles in hrflow
`Ceipal` :arrow_right: `HrFlow`

Send **created** 'profile(s)' _from_ Ceipal _to_ HrFlow



## Ceipal Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `string` | None | Email of the user to authenticate |
| `password` :red_circle: | `string` | None | Password of the user to authenticate |
| `api_key` :red_circle: | `string` | None | API key of the user to authenticate |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Ceipal)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `integer\|null` | 20 | Number of items to pull from CEIPAL |
| `created_by`  | `integer\|null` | None | User ID of the applicant creator |
| `source`  | `integer\|null` | None | Source ID of the applicants |
| `applicant_status`  | `integer\|null` | None | Applicant status ID |
| `sortorder`  | `string\|null` | None | Sort order (asc or desc) |
| `sortby`  | `string\|null` | None | Sort by field (e.g., applicant_id) |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `string` | None | HrFlow.ai source key |

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
from hrflow_connectors.v2 import Ceipal


logging.basicConfig(level=logging.INFO)


Ceipal.create_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        email=...,
        password=...,
        api_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        limit=...,
        created_by=...,
        source=...,
        applicant_status=...,
        sortorder=...,
        sortby=...,
    ),
    push_parameters=dict(
        source_key=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```