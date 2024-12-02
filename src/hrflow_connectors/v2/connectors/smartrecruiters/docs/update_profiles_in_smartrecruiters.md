# Update profiles in smartrecruiters
`HrFlow` :arrow_right: `SmartRecruiters`

Send **updated** 'profile(s)' _from_ HrFlow _to_ SmartRecruiters



**SmartRecruiters endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Update Candidate**](https://developers.smartrecruiters.com/reference/candidatespatch-1) | Endpoint to update a candidate with a given id, a candidateId parameter is required, the request method is `PUT` |


## SmartRecruiters Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `string` | None | X-SmartToken used to access SmartRecruiters API |

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

## Push Parameters (SmartRecruiters)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |

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


SmartRecruiters.update_profiles_in_smartrecruiters(
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
        source_key=...,
        profile_key=...,
    ),
    push_parameters=dict(
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```