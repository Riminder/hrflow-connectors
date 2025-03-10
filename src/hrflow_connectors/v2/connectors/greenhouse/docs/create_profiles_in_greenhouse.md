# Create profiles in greenhouse
`HrFlow` :arrow_right: `Greenhouse`

Send **created** 'profile(s)' _from_ HrFlow _to_ Greenhouse



**Greenhouse endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Post Candidate**](https://developers.greenhouse.io/job-board.html#jobs) | Endpoint to create a new candidate and assign to a talent pool, the request method is `POST` |


## Greenhouse Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth` :red_circle: | `string` | None | XAPIKeyAuth |

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

## Push Parameters (Greenhouse)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `on_behalf_of` :red_circle: | `string` | None | ID of the user issuing this request. Required for auditing purposes. |

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
from hrflow_connectors.v2 import Greenhouse


logging.basicConfig(level=logging.INFO)


Greenhouse.create_profiles_in_greenhouse(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        auth=...,
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
        on_behalf_of=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```