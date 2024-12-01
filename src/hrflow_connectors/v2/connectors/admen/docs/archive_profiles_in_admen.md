# Archive profiles in admen
`HrFlow` :arrow_right: `AD-MEN`

Send **archived** 'profile(s)' _from_ HrFlow _to_ AD-MEN



## AD-MEN Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `db_host` :red_circle: | `string` | None | The hostname of the database server |
| `db_port` :red_circle: | `integer` | None | The port of the database server |
| `db_name` :red_circle: | `string` | None | The name of the database |
| `db_user` :red_circle: | `string` | None | The username to connect to the database |
| `db_password` :red_circle: | `string` | None | The password to connect to the database |

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

## Push Parameters (AD-MEN)

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
from hrflow_connectors.v2 import Admen


logging.basicConfig(level=logging.INFO)


Admen.archive_profiles_in_admen(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        db_host=...,
        db_port=...,
        db_name=...,
        db_user=...,
        db_password=...,
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