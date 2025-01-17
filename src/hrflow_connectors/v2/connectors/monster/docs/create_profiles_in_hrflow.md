# Create profiles in hrflow
`Monster` :arrow_right: `HrFlow`

Send **created** 'profile(s)' _from_ Monster _to_ HrFlow



## Monster Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | Client ID that your Monster representative provided you with. |
| `client_secret` :red_circle: | `string` | None | Client Secret that your Monster representative provided you with. |
| `easy_apply_key`  | `string\|null` | None | Easy Apply API key that your Monster representative provided you with. |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Monster)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `searchType` :red_circle: | `Literal['JobDetail','Semantic']` | None | Defines if the user is providing parameters for the semantic search engine or matching with a job description/id. Possible values:
·'JobDetail'. Automated matching using Monster AI to construct a search based on a job title, job description and location.
·'Semantic'. Full function semantic search and hybrid semantic/Boolean search. |
| `semantic`  | `null\|object` | None | Semantic search parameters. |
| `JobDetail`  | `null\|object` | None | JobDetail search parameters. |
| `candidateName`  | `string\|null` | None | Name of a single candidate to look for. |

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
from hrflow_connectors.v2 import Monster


logging.basicConfig(level=logging.INFO)


Monster.create_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        easy_apply_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        searchType=...,
        semantic=...,
        JobDetail=...,
        candidateName=...,
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