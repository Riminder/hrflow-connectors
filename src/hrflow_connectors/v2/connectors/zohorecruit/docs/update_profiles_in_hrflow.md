# Update profiles in hrflow
`Zoho Recruit` :arrow_right: `HrFlow`

Send **updated** 'profile(s)' _from_ Zoho Recruit _to_ HrFlow



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

## Pull Parameters (Zoho Recruit)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `fields`  | `string\|null` | None | To list all the module records with respect to fields
Multiple field API names, comma-separated.
For example Last_Name, Email |
| `sort_order`  | `Literal['asc','desc']\|null` | None | To sort the available list of records in either ascending or descending order
asc - ascending order
desc - descending order |
| `sort_by`  | `string\|null` | None | To sort the available list of records based on the given field
Field API name
Example: Email |
| `cvid`  | `integer\|null` | None | To get the list of records based on custom views
{custom_view_id} |
| `territory_id`  | `integer\|null` | None | To get the list of records based on territory
{territory_id} |
| `include_child`  | `boolean\|null` | None | To include records from the child territories.
True includes child territory records.
False does not include child territory records.
The default value is false. |
| `state`  | `Literal['draft','save']\|null` | None | If the value of this parameter is 'draft', then the response will only contain Draft records from the specified module. If the parameter's value is 'save', then the response will return saved records from the specified module.

If this parameter is not included in your request body, then the response will only return saved records from the specified module. |
| `converted`  | `Literal['both','false','true']\|null` | false | To get the list of converted records.
The default value is false
true - get only converted records
false - get only non-converted records
both - get all records |
| `approved`  | `Literal['both','false','true']\|null` | true | To get the list of approved records.
The default value is true
true - get only approved records
false - get only non-approved records
both - get all records |

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
from hrflow_connectors.v2 import ZohoRecruit


logging.basicConfig(level=logging.INFO)


ZohoRecruit.update_profiles_in_hrflow(
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
        fields=...,
        sort_order=...,
        sort_by=...,
        cvid=...,
        territory_id=...,
        include_child=...,
        state=...,
        converted=...,
        approved=...,
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