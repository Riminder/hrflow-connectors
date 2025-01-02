# Create jobs in hrflow
`Greenhouse` :arrow_right: `HrFlow`

Send **created** 'job(s)' _from_ Greenhouse _to_ HrFlow


**Greenhouse endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Get job**](https://developers.greenhouse.io/harvest.html?shell#get-retrieve-job) | Endpoint to get the content of a job with a given id. The request method is `GET` |



## Greenhouse Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth` :red_circle: | `string` | None | XAPIKeyAuth |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Greenhouse)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `skip_count`  | `boolean\|null` | None | If true, the performance of retrieving jobs will improve. This will remove last from the link response header. |
| `created_before`  | `string\|null` | None | Return only jobs that were created before this timestamp. Timestamp must be in in ISO-8601 format. |
| `created_after`  | `string\|null` | None | Return only jobs that were created at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `updated_before`  | `string\|null` | None | Return only jobs that were updated before this timestamp. Timestamp must be in in ISO-8601 format. |
| `updated_after`  | `string\|null` | None | Return only jobs that were updated at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `requisition_id`  | `string\|null` | None | If included, will return only the jobs that match the given requisition_id |
| `opening_id`  | `string\|null` | None | If included, will return only the jobs that contain at least one opening with the given opening_id. |
| `status`  | `string\|null` | None | One of 'open', 'closed', or 'draft'. If included, will only return jobs with that status. |
| `department_id`  | `string\|null` | None | If included, will return only the jobs in this specific department. |
| `external_department_id`  | `string\|null` | None | This may be used instead of department_id and represents the ID of the department in an external system. |
| `office_id`  | `string\|null` | None | If included, will return only the jobs in this specific office. |
| `external_office_id`  | `string\|null` | None | This may be used instead of office_id and represents the ID of the office in an external system. |
| `custom_field_option_id`  | `string\|null` | None | The job contains a custom field with this custom_field_option_id selected. Option IDs can be retrieved from the GET Custom Field Options endpoint. |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |
| `enrich_with_parsing`  | `boolean` | False | When enabled jobs are enriched with HrFlow.ai parsing |

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


Greenhouse.create_jobs_in_hrflow(
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
        skip_count=...,
        created_before=...,
        created_after=...,
        updated_before=...,
        updated_after=...,
        requisition_id=...,
        opening_id=...,
        status=...,
        department_id=...,
        external_department_id=...,
        office_id=...,
        external_office_id=...,
        custom_field_option_id=...,
    ),
    push_parameters=dict(
        board_key=...,
        enrich_with_parsing=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```