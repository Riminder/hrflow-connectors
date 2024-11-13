# Archive profiles in hrflow
`Bullhorn` :arrow_right: `HrFlow`

Send **archived** 'profile(s)' _from_ Bullhorn _to_ HrFlow



## Bullhorn Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | Client identifier for Bullhorn |
| `client_secret` :red_circle: | `string` | None | Client secret identifier for Bullhorn |
| `password` :red_circle: | `string` | None | Password for Bullhorn login |
| `username` :red_circle: | `string` | None | Username for Bullhorn login |

## HrFlow Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Bullhorn)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `integer\|null` | None | Number of items to pull, ignored if not provided. |
| `last_modified_date` :red_circle: | `string` | None | The modification date from which you want to pull profiles |
| `query`  | `string` | isDeleted:0 | This query will restrict the results retrieved from Bullhorn based on the specified conditions |
| `fields`  | `string` | id | Field to be used as reference for archiving |

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
from hrflow_connectors.v2 import Bullhorn


logging.basicConfig(level=logging.INFO)


Bullhorn.archive_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        password=...,
        username=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        limit=...,
        last_modified_date=...,
        query=...,
        fields=...,
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