# Create profiles in taleez
`HrFlow` :arrow_right: `Taleez`

Send **created** 'profile(s)' _from_ HrFlow _to_ Taleez



## Taleez Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_taleez_api_secret` :red_circle: | `string` | None | X-taleez-api-secret used to access Taleez API |

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

## Push Parameters (Taleez)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `recruiterId`  | `integer\|null` | None | Id of the recruiter to associate the candidate with |
| `unitId`  | `integer\|null` | None | Id of the unit associated with the candidate. Only specified for companies with multiple units and "candidate segmentation by unit" setting. |
| `job_ids`  | `array\|null` | None | List of job ids to associate the candidate with |
| `pool_ids`  | `array\|null` | None | List of pool ids to associate the candidate with |

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
from hrflow_connectors.v2 import Taleez


logging.basicConfig(level=logging.INFO)


Taleez.create_profiles_in_taleez(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        x_taleez_api_secret=...,
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
        recruiterId=...,
        unitId=...,
        job_ids=...,
        pool_ids=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```