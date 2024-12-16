# Update profiles in breezyhr
`HrFlow` :arrow_right: `Breezy HR`

Send **updated** 'profile(s)' _from_ HrFlow _to_ Breezy HR



## Breezy HR Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `string` | None | email |
| `password` :red_circle: | `string` | None | password |
| `company_id`  | `string\|null` | None | ID of company to pull jobs from in Breezy HR database associated with the authenticated user |
| `company_name`  | `string\|null` | None | [⚠️ Requiered if company_id is not specified], the company associated with the authenticated user |

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

## Push Parameters (Breezy HR)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `position_id` :red_circle: | `string` | None | Id of the position to create a new candidate for |

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
from hrflow_connectors.v2 import BreezyHR


logging.basicConfig(level=logging.INFO)


BreezyHR.update_profiles_in_breezyhr(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        email=...,
        password=...,
        company_id=...,
        company_name=...,
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
        position_id=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```