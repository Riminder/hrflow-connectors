# Create profiles in talentsoft
`HrFlow` :arrow_right: `TalentSoft`

Send **created** 'profile(s)' _from_ HrFlow _to_ TalentSoft



## TalentSoft Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | client id used to access TalentSoft front office API |
| `client_secret` :red_circle: | `string` | None | client secret used to access TalentSoft front office API |
| `client_url` :red_circle: | `string` | None | url used to access TalentSoft front office API |

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

## Push Parameters (TalentSoft)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `job_reference` :red_circle: | `string` | None | reference of the job offer to which the candidate is applying |

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
from hrflow_connectors.v2 import TalentSoft


logging.basicConfig(level=logging.INFO)


TalentSoft.create_profiles_in_talentsoft(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        client_url=...,
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
        job_reference=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```