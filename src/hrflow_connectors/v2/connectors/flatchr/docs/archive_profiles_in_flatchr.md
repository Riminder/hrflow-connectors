# Archive profiles in flatchr
`HrFlow` :arrow_right: `Flatchr`

Send **archived** 'profile(s)' _from_ HrFlow _to_ Flatchr



**Flatchr endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Archive Candidate**](https://developers.flatchr.io/docs/QuickStart/Candidats/Archiver_un_candidat) | Archive a candidate |


## Flatchr Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_key` :red_circle: | `string` | None | The API key to authenticate with the Flatchr API |
| `company_id` :red_circle: | `string` | None | The ID of the company to authenticate with |
| `env_base_url`  | `Literal['https://api.demo.flatchr.io','https://api.flatchr.io/']` | https://api.flatchr.io/ | The base URL of the Flatchr API |

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

## Push Parameters (Flatchr)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `vacancy_id`  | `string\|null` | None | The ID of the offer to assign the candidate to
Equivalent to id in the Flatchr API not vacancy_id nor the slug |

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
from hrflow_connectors.v2 import Flatchr


logging.basicConfig(level=logging.INFO)


Flatchr.archive_profiles_in_flatchr(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_key=...,
        company_id=...,
        env_base_url=...,
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
        vacancy_id=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```