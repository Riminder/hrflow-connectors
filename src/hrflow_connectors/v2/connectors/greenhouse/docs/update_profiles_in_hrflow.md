# Update profiles in hrflow
`Greenhouse` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ Greenhouse _to_ HrFlow



## Greenhouse Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth` :red_circle: | `string` | None | XAPIKeyAuth |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Greenhouse)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `skip_count`  | `boolean\|null` | None | If true, the performance of retrieving candidates will improve. This will remove last from the link response header. |
| `created_before`  | `string\|null` | None | Return only candidates that were created before this timestamp. Timestamp must be in in ISO-8601 format. |
| `created_after`  | `string\|null` | None | Return only candidates that were created at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `updated_before`  | `string\|null` | None | Return only candidates that were updated before this timestamp. Timestamp must be in in ISO-8601 format. |
| `updated_after`  | `string\|null` | None | Return only candidates that were updated at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `job_id`  | `string\|null` | None | Only returns candidates who have applied to the specified job. Prospects on the job are not included. |
| `email`  | `string\|null` | None | If supplied, only return candidates who have a matching e-mail address. If supplied with job_id, only return a candidate with a matching e-mail with an application on the job. If email and candidate_ids are included, candidate_ids will be ignored. |
| `candidate_ids`  | `string\|null` | None | If supplied, return only the candidates with the given ids. These are supplied as a comma separated string. e.g.: 'candidate_ids=123,456,789'. When combined with job_id, only return candidates with an application on the job. A maximum of 50 candidates can be returned this way. |

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
from hrflow_connectors.v2 import Greenhouse


logging.basicConfig(level=logging.INFO)


Greenhouse.update_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        auth=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        skip_count=...,
        created_before=...,
        created_after=...,
        updated_before=...,
        updated_after=...,
        job_id=...,
        email=...,
        candidate_ids=...,
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