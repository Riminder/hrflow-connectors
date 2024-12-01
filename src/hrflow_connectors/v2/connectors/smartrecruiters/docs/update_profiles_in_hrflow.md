# Update profiles in hrflow
`SmartRecruiters` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ SmartRecruiters _to_ HrFlow


**SmartRecruiters endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Get candidate**](https://developers.smartrecruiters.com/reference/candidatesget-1) | Endpoint to get the content of a candidate with a given id, a candidateId parameter is required, the request method is `GET` |



## SmartRecruiters Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `string` | None | X-SmartToken used to access SmartRecruiters API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (SmartRecruiters)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `query`  | `string\|null` | None | keyword search, for more infromation see SmartRecruiters HelpCenter |
| `job_ids`  | `array\|null` | None | List of job ids to filter candidates by |
| `tags`  | `array\|null` | None | List of tags to filter candidates by |
| `onboarding_status`  | `Literal['ONBOARDING_FAILED','ONBOARDING_SUCCESSFUL','READY_TO_ONBOARD']\|null` | None | Onboarding status of a candidate |
| `status`  | `array\|null` | None | candidate’s status filter in a context of a job |
| `limit`  | `integer` | 100 | Number of items to pull from SmartRecruiters at a time. Not matter what value is supplied it is capped at 100 |
| `updated_after`  | `string\|null` | None | ISO8601-formatted time boundaries for the candidate update time |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `string` | None | HrFlow.ai source key |
| `only_edit_fields`  | `array\|null` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

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
from hrflow_connectors.v2 import SmartRecruiters


logging.basicConfig(level=logging.INFO)


SmartRecruiters.update_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        x_smart_token=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        query=...,
        job_ids=...,
        tags=...,
        onboarding_status=...,
        status=...,
        limit=...,
        updated_after=...,
    ),
    push_parameters=dict(
        source_key=...,
        only_edit_fields=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```