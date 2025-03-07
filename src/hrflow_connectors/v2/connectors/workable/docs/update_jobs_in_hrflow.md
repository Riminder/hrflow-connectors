# Update jobs in hrflow
`Workable` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Workable _to_ HrFlow



## Workable Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_access_token` :red_circle: | `string` | None | The API access token for the Workable account, which can be generated from the Integrations section in the Workable backend settings. |
| `subdomain` :red_circle: | `string` | None | The subdomain of the Workable account, can be retrieved with a GET request to the /account endpoint |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Workable)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `state`  | `Literal['archived','closed','draft','published']\|null` | None | Returns jobs with the current state. Possible values (draft, published, archived & closed). |
| `since_id`  | `string\|null` | None | Returns jobs with ID greater than the specified value. |
| `max_id`  | `string\|null` | None | Returns jobs with ID less than the specified value. |
| `created_after`  | `string\|integer\|null` | None | Returns jobs created after the specified timestamp/date time. |
| `updated_after`  | `string\|integer\|null` | None | Returns jobs updated after the specified timestamp/date time. |

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
from hrflow_connectors.v2 import Workable


logging.basicConfig(level=logging.INFO)


Workable.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_access_token=...,
        subdomain=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        state=...,
        since_id=...,
        max_id=...,
        created_after=...,
        updated_after=...,
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