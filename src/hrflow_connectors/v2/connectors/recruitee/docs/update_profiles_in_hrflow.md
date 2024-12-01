# Update profiles in hrflow
`Recruitee` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ Recruitee _to_ HrFlow



## Recruitee Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `string` | None | Company ID. A company subdomain can also be used. |
| `api_token` :red_circle: | `string` | None | Personal API Token allowing access to the Recruitee API from external services. |
| `recruitee_endpoint`  | `Literal['https://api.rc.recruitee.dev/c','https://api.recruitee.com/c','https://api.s.recruitee.com/c']` | https://api.recruitee.com/c | Specifies which endpoint to be used, satging or production. |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Recruitee)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `integer\|null` | None | Specifies the number of candidates to retrieve |
| `offset`  | `integer\|null` | None | Skip number of candidates from the begining, used for ‘load more’, offset for next page should be current offset + limit |
| `created_after`  | `string\|null` | None | Show only candidates created after given date |
| `disqualified`  | `boolean\|null` | None | Show only disqualified candidates who are disqualified in at least one job (should be string ‘true’ or ‘1’). |
| `qualified`  | `boolean\|null` | None | Show only disqualified candidates who are qualified in at least one job (should be string ‘true’ or ‘1’). |
| `ids`  | `string\|null` | None | List of IDs separated by comma, example: 234221,4211412,535432 |
| `offer_id`  | `string\|null` | None | Filter by offer |
| `query`  | `string\|null` | None | Search query for candidate’s name or offer |
| `sort`  | `Literal['by_date','by_last_message']\|null` | None | Sorting options: by_date, by_last_message |
| `with_messages`  | `boolean\|null` | None | Show only candidates with messages (should be string ‘true’ or ‘1’) |
| `with_my_messages`  | `boolean\|null` | None | Show only candidates with messages that current admin sent (should be string ‘true’ or ‘1’ |

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
from hrflow_connectors.v2 import Recruitee


logging.basicConfig(level=logging.INFO)


Recruitee.update_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        company_id=...,
        api_token=...,
        recruitee_endpoint=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        limit=...,
        offset=...,
        created_after=...,
        disqualified=...,
        qualified=...,
        ids=...,
        offer_id=...,
        query=...,
        sort=...,
        with_messages=...,
        with_my_messages=...,
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