# Archive jobs in hrflow
`Teamtailor` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ Teamtailor _to_ HrFlow



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
| `filter_status`  | `Literal['all','archived','draft','published','scheduled','unlisted']\|null` | None | Filter by job status. Available statuses: ['published', 'unlisted', 'archived', 'draft', 'scheduled', 'all'] |
| `filter_feed`  | `Literal['internal','public']\|null` | None | Status of a job. One of ['public', 'internal'] |
| `filter_department`  | `string\|null` | None | Filter by department id |
| `filter_role`  | `string\|null` | None | Filter by role id |
| `filter_locations`  | `string\|null` | None | Filter by location id |
| `filter_regions`  | `string\|null` | None | Filter by region id |
| `filter_tags`  | `string\|null` | None | Filter by tags |
| `filter_remote_status`  | `Literal['fully','hybrid','none','temporary']\|null` | None | Filter by remote status. Available remote statuses: ['none', 'hybrid', 'temporary', 'fully'] |
| `filter_created_at_from`  | `string\|null` | None | Filter by created-at older than this date. |
| `filter_created_at_to`  | `string\|null` | None | Filter by created-at newer than this date. |
| `filter_updated_at_from`  | `string\|null` | None | Filter by updated-at older than this date. |
| `filter_updated_at_to`  | `string\|null` | None | Filter by updated-at newer than this date. |
| `sort`  | `string\|null` | None | Default sorting based on pinned status and publish date.
'-pinned,date' |

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
from hrflow_connectors.v2 import Teamtailor


logging.basicConfig(level=logging.INFO)


Teamtailor.archive_jobs_in_hrflow(
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
        filter_status=...,
        filter_feed=...,
        filter_department=...,
        filter_role=...,
        filter_locations=...,
        filter_regions=...,
        filter_tags=...,
        filter_remote_status=...,
        filter_created_at_from=...,
        filter_created_at_to=...,
        filter_updated_at_from=...,
        filter_updated_at_to=...,
        sort=...,
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