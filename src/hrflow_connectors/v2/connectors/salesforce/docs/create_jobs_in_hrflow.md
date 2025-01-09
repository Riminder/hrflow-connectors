# Create jobs in hrflow
`Salesforce` :arrow_right: `HrFlow`

Send **created** 'job(s)' _from_ Salesforce _to_ HrFlow



## Salesforce Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `sf_username` :red_circle: | `string` | None | username used to access Salesforce API |
| `sf_password` :red_circle: | `string` | None | password used to access Salesforce API |
| `sf_security_token` :red_circle: | `string` | None | Security Token to access Salesforce API.See below for instructions: How Can I Find My Security Token and Use It in Data Loader | Salesforce Platform  https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport |
| `sf_organization_id` :red_circle: | `string` | None | See below for instructions: How to find your organization id  https://help.salesforce.com/s/articleView?id=000385215&type=1 |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Salesforce)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `last_modified_date`  | `string\|null` | None | Last modified date |
| `limit`  | `integer` | 1000 | Total number of items to pull from Salesforce.By default limiting to 1000 |

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
from hrflow_connectors.v2 import Salesforce


logging.basicConfig(level=logging.INFO)


Salesforce.create_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        sf_username=...,
        sf_password=...,
        sf_security_token=...,
        sf_organization_id=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        last_modified_date=...,
        limit=...,
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