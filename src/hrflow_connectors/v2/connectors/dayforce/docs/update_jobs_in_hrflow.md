# Update jobs in hrflow
`Dayforce` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Dayforce _to_ HrFlow



## Dayforce Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `subdomain` :red_circle: | `string` | None | Subdomain used to access Ceridian API |
| `client_name_space` :red_circle: | `string` | None | Client name space used to access Ceridian API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Dayforce)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `companyName`  | `string\|null` | None | Company name. Example: XYZ Co. |
| `parentCompanyName`  | `string\|null` | None | Parent Company name. Example: Ceridian |
| `internalJobBoardCode`  | `string\|null` | None | XRefCode of Job Board. Example: CANDIDATEPORTAL |
| `includeActivePostingOnly`  | `boolean\|null` | None | If true, then exclude inactive postings from the result. If False, then the 'Last Update Time From' and 'Last Update Time To' parameters are required and the range specified between the 'Last Update Time From' and 'Last Update Time To' parameters must not be larger than 1 month. Example: True |
| `lastUpdateTimeFrom`  | `string\|null` | None | A starting timestamp of job posting date. Example: 2017-01-01T13:24:56 |
| `lastUpdateTimeTo`  | `string\|null` | None | An ending timestamp of last updated job posting. Example: 2017-02-01T13:24:56 |
| `datePostedFrom`  | `string\|null` | None | A starting timestamp of job posting date. Example: 2017-01-01T13:24:56 |
| `datePostedTo`  | `string\|null` | None | An ending timestamp of job posting date. Example: 2017-02-01T13:24:56 |
| `htmlDescription`  | `boolean\|null` | None | A flag to feed the jobs over with html formatting or plain text description |

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
from hrflow_connectors.v2 import Dayforce


logging.basicConfig(level=logging.INFO)


Dayforce.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        subdomain=...,
        client_name_space=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        companyName=...,
        parentCompanyName=...,
        internalJobBoardCode=...,
        includeActivePostingOnly=...,
        lastUpdateTimeFrom=...,
        lastUpdateTimeTo=...,
        datePostedFrom=...,
        datePostedTo=...,
        htmlDescription=...,
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