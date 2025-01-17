# Archive jobs in hrflow
`Ceipal` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ Ceipal _to_ HrFlow



## Ceipal Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `string` | None | Email of the user to authenticate |
| `password` :red_circle: | `string` | None | Password of the user to authenticate |
| `api_key` :red_circle: | `string` | None | API key of the user to authenticate |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Ceipal)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `integer\|null` | 20 | Number of items to pull from CEIPAL |
| `business_unit_id`  | `integer\|null` | None | Every job is associated with a business unit. Pass the business unit id as the parameter to get the jobs. |
| `country`  | `integer\|null` | None | Pull the jobs based on the country. Use the countries endpoint to get the list of countries. |
| `state`  | `integer\|null` | None | Pass the state id as the parameter to get the jobs. Use the states endpoint to get the states list. |
| `job_status`  | `integer\|null` | None | Use the job status endpoint from the master data to get the job statuses. Pass the id here to pull the matching jobs. |
| `post_on_careerportal`  | `boolean\|null` | None | Send 1 to get all the jobs that are posted on the careers page. 0 for the jobs that are not posted. |
| `fromdate`  | `string\|null` | None | To get the jobs in between the dates, use this parameter (date format: mm-dd-yyyy) |
| `todate`  | `string\|null` | None | This parameter works along with the fromdate. Gives the jobs that are created between the from date and to date (date format: mm-dd-yyyy) |
| `filter`  | `string\|null` | None | When the from date and to dates are used, this parameter is mandatory. Use ‘created’ as the filter value to get the jobs that are created between the from and to dates. |
| `posted_ago_days`  | `integer\|null` | None | Pass any numeric value to get the jobs that are created within this number of days. |
| `sortorder`  | `string\|null` | None | Use either asc or desc to sort the job postings list. |
| `sortby`  | `string\|null` | None | This filter is used along with the sortorder. Job postings can be sorted based on the job code, created date, modified date. |
| `assigned_recruiter`  | `object\|null` | None | Pass the assigned recruiter as a dictionary. |

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
from hrflow_connectors.v2 import Ceipal


logging.basicConfig(level=logging.INFO)


Ceipal.archive_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        email=...,
        password=...,
        api_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        limit=...,
        business_unit_id=...,
        country=...,
        state=...,
        job_status=...,
        post_on_careerportal=...,
        fromdate=...,
        todate=...,
        filter=...,
        posted_ago_days=...,
        sortorder=...,
        sortby=...,
        assigned_recruiter=...,
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