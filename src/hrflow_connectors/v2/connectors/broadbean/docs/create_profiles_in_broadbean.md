# Create profiles in broadbean
`HrFlow` :arrow_right: `Broadbean`

Send **created** 'profile(s)' _from_ HrFlow _to_ Broadbean



**Broadbean endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Send candidate**](https://integrations.broadbean.com/hc/en-us/articles/115004599865-Sending-a-Candidate-in-) | This route accepts a POST request consisting of a JSON payload and some authentication Headers. The response will consist of a success flag and either a transaction ID (success) or an error. |


## Broadbean Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `secret_key` :red_circle: | `string` | None | Secret provided by Veritone Hire |

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

## Push Parameters (Broadbean)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | ID for the candidate importer client mapper (Provided by Veritone Hire) |
| `source_id` :red_circle: | `string` | None | Transaction/Publisher Source ID to link to the transformer (Provided by Veritone Hire) |
| `job_id` :red_circle: | `string` | None | The ID of the Job the candidate has applied to |
| `shortlist_id`  | `string\|null` | None | Shortlist ID for the candidate	 |
| `aplitrak_email_address`  | `string\|null` | None | If you are a Job Board integrating with Candidate Hub, it is important that you include the unique tracking link/email in the supplementary context. This helps us identify the vacancy the candidate has applied for.	 |

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
from hrflow_connectors.v2 import Broadbean


logging.basicConfig(level=logging.INFO)


Broadbean.create_profiles_in_broadbean(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        secret_key=...,
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
        client_id=...,
        source_id=...,
        job_id=...,
        shortlist_id=...,
        aplitrak_email_address=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```