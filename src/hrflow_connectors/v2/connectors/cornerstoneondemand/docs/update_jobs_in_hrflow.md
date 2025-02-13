# Update jobs in hrflow
`Cornerstone OnDemand` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Cornerstone OnDemand _to_ HrFlow



## Cornerstone OnDemand Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `corporation_name` :red_circle: | `string` | None | The name of the corporation that is registered in Cornerstone OnDemand. |
| `client_id` :red_circle: | `string` | None | The client ID of the application that is registered in Cornerstone OnDemand. |
| `client_secret` :red_circle: | `string` | None | The client secret of the application that is registered in Cornerstone OnDemand. |
| `environment` :red_circle: | `Literal['pilot','production','stage']` | None | The environment in which the application is registered in Cornerstone OnDemand. Possible values are 'stage', 'pilot', and 'production'. |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Cornerstone OnDemand)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `Title`  | `string\|null` | None | Job Title. |
| `ReqId`  | `string\|null` | None | Requisition ID. |
| `FromDate`  | `string\|null` | None | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss |
| `ToDate`  | `string\|null` | None | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss |
| `lastModifiedSince`  | `string\|null` | None | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss |
| `DivisionId`  | `string\|null` | None | Division ID. |
| `LocationId`  | `string\|null` | None | Location ID. |
| `Statuses`  | `string\|null` | None | Comma separated list of statuses. e.g. "Draft,Open,Closed".
Enum: "Draft" "Open" "Closed" "Cancelled" "Pending Approval" "Approval Denied" "Open - Pending Re - Approval" "On Hold" |
| `Language`  | `string\|null` | None | Language should include ISO language code. Example en-US, fr-FR, it-IT, en-GB... |
| `limit`  | `integer\|null` | 100 | The maximum number of records to return. |

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
from hrflow_connectors.v2 import CornerstoneOnDemand


logging.basicConfig(level=logging.INFO)


CornerstoneOnDemand.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        corporation_name=...,
        client_id=...,
        client_secret=...,
        environment=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        Title=...,
        ReqId=...,
        FromDate=...,
        ToDate=...,
        lastModifiedSince=...,
        DivisionId=...,
        LocationId=...,
        Statuses=...,
        Language=...,
        limit=...,
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