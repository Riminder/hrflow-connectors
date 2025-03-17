# Create profiles in hrflow
`Cornerstone OnDemand` :arrow_right: `HrFlow`

Send **created** 'profile(s)' _from_ Cornerstone OnDemand _to_ HrFlow



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
| `CurrentStatus` :red_circle: | `string` | None | Valid Status. Enum: ['New Submission', 'In Review', 'Phone Screening', 'Interview', 'Background Check', 'Offer Letter', 'Closed', 'Hired'] |
| `StatusDate`  | `string\|null` | None | Local Datetime Value. Format should be yyyy-mm-dd |
| `CsodGUID`  | `string\|null` | None | Applicant's GUID Value. |
| `FirstName`  | `string\|null` | None | Applicant's First Name. |
| `LastName`  | `string\|null` | None | Applicant's Last Name. |
| `RequisitionID`  | `string\|null` | None | Job Requisition Id. |
| `JobTitle`  | `string\|null` | None | Job Requision Title. |
| `limit`  | `integer\|null` | 100 | The maximum number of records to return. |

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
from hrflow_connectors.v2 import CornerstoneOnDemand


logging.basicConfig(level=logging.INFO)


CornerstoneOnDemand.create_profiles_in_hrflow(
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
        CurrentStatus=...,
        StatusDate=...,
        CsodGUID=...,
        FirstName=...,
        LastName=...,
        RequisitionID=...,
        JobTitle=...,
        limit=...,
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