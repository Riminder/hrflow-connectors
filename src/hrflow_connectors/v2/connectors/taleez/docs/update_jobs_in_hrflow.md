# Update jobs in hrflow
`Taleez` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Taleez _to_ HrFlow



## Taleez Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_taleez_api_secret` :red_circle: | `string` | None | X-taleez-api-secret used to access Taleez API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Taleez)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `unitId`  | `array\|null` | None | Filter on job unit. Can be used with others filters. |
| `status`  | `array\|null` | None | Filter on job status. Can be used with others filters.
Available values : ['DRAFT', 'PUBLISHED', 'DONE', 'SUSPENDED'] |
| `contract`  | `array\|null` | None | Filter on job contract. Can be used with others filters. |
| `city`  | `array\|null` | None | Filter on job city. Can be used with others filters. |
| `companyLabel`  | `array\|null` | None | Filter on job company label (strict search). Can be used with others filters. |
| `tag`  | `array\|null` | None | Filter on job tag. Can be used with others filters. |
| `visibility`  | `array\|null` | None | Filter on job visibility. Can be used with others filters. |
| `visibilityToken`  | `string\|null` | None | Secret token for restricted jobs. |
| `sort`  | `string\|null` | None | Sort the list by one or multiple params. Ex : sort=dateCreation.desc,label.asc |

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
from hrflow_connectors.v2 import Taleez


logging.basicConfig(level=logging.INFO)


Taleez.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        x_taleez_api_secret=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        unitId=...,
        status=...,
        contract=...,
        city=...,
        companyLabel=...,
        tag=...,
        visibility=...,
        visibilityToken=...,
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