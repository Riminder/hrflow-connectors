# Create profiles in zohorecruit
`HrFlow` :arrow_right: `Zoho Recruit`

Send **created** 'profile(s)' _from_ HrFlow _to_ Zoho Recruit



## Zoho Recruit Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | A unique ID displayed under Self Client > Client Secret. |
| `client_secret` :red_circle: | `string` | None | A unique confidential secret displayed under Self Client > Client Secret. |
| `authorization_code`  | `string\|null` | None | The authorization code generated during the Self Client creation, used to get the refresh token and the first access token. |
| `refresh_token`  | `string\|null` | None | The refresh token is used to generate a new access token when the current access token expires. |
| `zoho_accounts_url`  | `Literal['https://accounts.zoho.com','https://accounts.zoho.com.au','https://accounts.zoho.com.cn','https://accounts.zoho.eu','https://accounts.zoho.in','https://accounts.zoho.jp','https://accounts.zohocloud.ca']` | https://accounts.zoho.eu | Zoho CRM is hosted at multiple data centers. Therefore, the API domain URL varies for each data center.
You must use your domain-specific Zoho Accounts URL to generate access and refresh tokens. The following are the various domains and their corresponding accounts URLs.
 US: https://accounts.zoho.com
 AU: https://accounts.zoho.com.au
 EU: https://accounts.zoho.eu
 IN: https://accounts.zoho.in
 CN: https://accounts.zoho.com.cn
 JP: https://accounts.zoho.jp
 CA: https://accounts.zohocloud.ca |

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

## Push Parameters (Zoho Recruit)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |

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
from hrflow_connectors.v2 import ZohoRecruit


logging.basicConfig(level=logging.INFO)


ZohoRecruit.create_profiles_in_zohorecruit(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        authorization_code=...,
        refresh_token=...,
        zoho_accounts_url=...,
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
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```