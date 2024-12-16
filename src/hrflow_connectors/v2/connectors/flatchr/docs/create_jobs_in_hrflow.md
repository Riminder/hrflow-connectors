# Create jobs in hrflow
`Flatchr` :arrow_right: `HrFlow`

Send **created** 'job(s)' _from_ Flatchr _to_ HrFlow


**Flatchr endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Get Vacancies**](https://developers.flatchr.io/docs/QuickStart/Annonces/Recuperer_l_ensemble_de_vos_annonces) | search for one or more vacancies |



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
from hrflow_connectors.v2 import Flatchr


logging.basicConfig(level=logging.INFO)


Flatchr.create_jobs_in_hrflow(
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