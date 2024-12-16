# Update profiles in hrflow
`Flatchr` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ Flatchr _to_ HrFlow



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

## Pull Parameters (Flatchr)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `firstname`  | `string\|null` | None | The firstname of the candidate to search for |
| `lastname`  | `string\|null` | None | The lastname of the candidate to search for |
| `email`  | `string\|null` | None | The email of the candidate to search for |
| `hired`  | `boolean\|null` | None | Whether the candidate has been hired or not |
| `column`  | `string\|null` | None | The column in which the candidate is located, Ex: 'Entretien RH' |
| `start`  | `string\|null` | None | The start date in MM/DD/YY of the search |
| `end`  | `string\|null` | None | The end date in MM/DD/YY of the search |
| `vacancy`  | `string\|null` | None | id of the offer in which the candidate is involved |

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
from hrflow_connectors.v2 import Flatchr


logging.basicConfig(level=logging.INFO)


Flatchr.update_profiles_in_hrflow(
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
        firstname=...,
        lastname=...,
        email=...,
        hired=...,
        column=...,
        start=...,
        end=...,
        vacancy=...,
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