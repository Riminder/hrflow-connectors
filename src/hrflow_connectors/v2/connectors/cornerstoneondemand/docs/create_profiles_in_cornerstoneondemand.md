# Create profiles in cornerstoneondemand
`HrFlow` :arrow_right: `Cornerstone OnDemand`

Send **created** 'profile(s)' _from_ HrFlow _to_ Cornerstone OnDemand



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

## Pull Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `string` | None | HrFlow.ai source key |
| `profile_key` :red_circle: | `string` | None | HrFlow.ai profile key |

## Push Parameters (Cornerstone OnDemand)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `jobRequisitionId` :red_circle: | `string` | None | The ATS job requisition’s identifier. This is a "ref" value and not the internal "id". A correct example is Req123. |
| `futureOpportunityOptIn`  | `boolean\|null` | None | Candidate’s decision to be considered for other positions. |
| `source`  | `null\|object` | None | How the application was submitted. If this object is null, we will default the submissionSource to Candidate API. |
| `questions`  | `array\|null` | None | A collection of application submission data for questions of type: Disclaimer, Compliance and Prescreening. |
| `sendEmail`  | `boolean` | True | Default: true
This is an optional field allowing an API consumer to specify if the candidate should or should not receive an email upon successful submission of their application. When set sendEmail is true, the applicant will receive the apply as guest, or standard application submission email. This will be dependent on whether the candidate's profile has or has not already been claimed. Some clients also require applicants to claim their email address before successfully claiming their account. This workflow will remain the same. If left empty, this property will default to true. |

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


CornerstoneOnDemand.create_profiles_in_cornerstoneondemand(
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
        source_key=...,
        profile_key=...,
    ),
    push_parameters=dict(
        jobRequisitionId=...,
        futureOpportunityOptIn=...,
        source=...,
        questions=...,
        sendEmail=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```