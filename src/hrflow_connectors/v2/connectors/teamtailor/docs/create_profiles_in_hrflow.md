# Create profiles in hrflow
`Teamtailor` :arrow_right: `HrFlow`

Send **created** 'profile(s)' _from_ Teamtailor _to_ HrFlow



## Teamtailor Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_key` :red_circle: | `string` | None | API key for authenticating with the Teamtailor API. You can generate it in the Teamtailor app under Settings > Integrations > API Keys. |
| `X_Api_Version`  | `string` | 20240404 | API version for the Teamtailor API. Default is '20240404'. |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Teamtailor)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `filter_email`  | `string\|null` | None | Filter by email address |
| `filter_department`  | `string\|null` | None | Filter by department |
| `filter_role`  | `string\|null` | None | Filter by role |
| `filter_locations`  | `string\|null` | None | Filter by location |
| `filter_regions`  | `string\|null` | None | Filter by region |
| `filter_created_at_from`  | `string\|null` | None | Filter by created-at older than this date. |
| `filter_created_at_to`  | `string\|null` | None | Filter by created-at newer than this date. |
| `filter_updated_at_from`  | `string\|null` | None | Filter by updated-at older than this date. |
| `filter_updated_at_to`  | `string\|null` | None | Filter by updated-at newer than this date. |
| `filter_connected`  | `Literal['false','true']\|null` | None | Filter candidates who has connected. |
| `sort`  | `string\|null` | None | Sort list by any of the candidate attributes. Use -id to sort by id descending. |

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
from hrflow_connectors.v2 import Teamtailor


logging.basicConfig(level=logging.INFO)


Teamtailor.create_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_key=...,
        X_Api_Version=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        filter_email=...,
        filter_department=...,
        filter_role=...,
        filter_locations=...,
        filter_regions=...,
        filter_created_at_from=...,
        filter_created_at_to=...,
        filter_updated_at_from=...,
        filter_updated_at_to=...,
        filter_connected=...,
        sort=...,
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