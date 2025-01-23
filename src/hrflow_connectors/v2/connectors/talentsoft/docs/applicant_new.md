# Applicant new
`TalentSoft` :arrow_right: `HrFlow`

Send **created** 'profile(s)' _from_ TalentSoft _to_ HrFlow



## TalentSoft Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | client id used to access TalentSoft front office API |
| `client_secret` :red_circle: | `string` | None | client secret used to access TalentSoft front office API |
| `client_url` :red_circle: | `string` | None | url used to access TalentSoft front office API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (TalentSoft)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `filter`  | `string\|null` | None | Filter to apply when reading profiles. See documentation at https://developers.cegid.com/api-details#api=cegid-talentsoft-recruiting-matchingindexation&operation=api-exports-v1-candidates-get . Examples : By id Single Item 'id::_TS-00001'; By id Multiple Items 'id::_TS-00001,_TS-00002'; By email 'email::john.doe@company.corp'; By updateDate updated before the 10th of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber greater than 108921  'chronoNumber:gt:108921' |

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
from hrflow_connectors.v2 import TalentSoft


logging.basicConfig(level=logging.INFO)


TalentSoft.applicant_new(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        client_url=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        filter=...,
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