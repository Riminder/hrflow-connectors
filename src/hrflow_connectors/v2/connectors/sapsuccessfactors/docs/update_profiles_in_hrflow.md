# Update profiles in hrflow
`SAP SuccessFactors` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ SAP SuccessFactors _to_ HrFlow



## SAP SuccessFactors Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_server` :red_circle: | `string` | None | Server to be accessed |
| `api_key` :red_circle: | `string` | None | API Key used to authenticate on the SAP API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (SAP SuccessFactors)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `$top`  | `integer\|null` | 100 | Show only the first n items value is capped at 100 |
| `$skip`  | `integer\|null` | None | Skip the first n items |
| `$search`  | `string\|null` | None | Search items by search phrases |
| `$filter`  | `string\|null` | None | Filter items by property values |
| `$count`  | `boolean\|null` | None | Include count of items |
| `$orderby`  | `array\|null` | None | Order items by property values
Available values : address, address desc, address2, address2 desc, agreeToPrivacyStatement, agreeToPrivacyStatement desc, anonymized, anonymized desc, anonymizedDateTime, anonymizedDateTime desc, candidateId, candidateId desc, candidateLocale, candidateLocale desc, cellPhone, cellPhone desc, city, city desc, contactEmail, contactEmail desc, country, country desc, creationDateTime, creationDateTime desc, currentTitle, currentTitle desc, dataPrivacyId, dataPrivacyId desc, dateofAvailability, dateofAvailability desc, externalCandidate, externalCandidate desc, firstName, firstName desc, homePhone, homePhone desc, lastLoginDateTime, lastLoginDateTime desc, lastModifiedDateTime, lastModifiedDateTime desc, lastName, lastName desc, middleName, middleName desc, partnerMemberId, partnerMemberId desc, partnerSource, partnerSource desc, password, password desc, primaryEmail, primaryEmail desc, privacyAcceptDateTime, privacyAcceptDateTime desc, publicIntranet, publicIntranet desc, shareProfile, shareProfile desc, usersSysId, usersSysId desc, visibilityOption, visibilityOption desc, zip, zip desc |

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
from hrflow_connectors.v2 import SAPSuccessFactors


logging.basicConfig(level=logging.INFO)


SAPSuccessFactors.update_profiles_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_server=...,
        api_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        $top=...,
        $skip=...,
        $search=...,
        $filter=...,
        $count=...,
        $orderby=...,
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