# Pull job list
`Zoho Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Zoho Recruit and writes them to an Hrflow.ai board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job_opening_to_hrflow`](../connector.py#L75) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `fields`  | `str` | None | To list all the module records with respect to fields
Multiple field API names, comma-separated.
For example Last_Name, Email |
| `sort_order`  | `str` | None | To sort the available list of records in either ascending or descending order
asc - ascending order
desc - descending order |
| `sort_by`  | `str` | None | To sort the available list of records based on the given field
Field API name
Example: Email |
| `converted`  | `str` | ZohoBool.FALSE | To get the list of converted records.
The default value is false
true - get only converted records
false - get only non-converted records
both - get all records |
| `approved`  | `str` | ZohoBool.TRUE | To get the list of approved records.
The default value is true
true - get only approved records
false - get only non-approved records
both - get all records |
| `cvid`  | `int` | None | To get the list of records based on custom views
{custom_view_id} |
| `territory_id`  | `int` | None | To get the list of records based on territory
{territory_id} |
| `include_child`  | `bool` | None | To include records from the child territories.
True includes child territory records.
False does not include child territory records.
The default value is false. |
| `state`  | `str` | None | If the value of this parameter is 'draft', then the response will only contain Draft records from the specified module. If the parameter's value is 'save', then the response will return saved records from the specified module.

If this parameter is not included in your request body, then the response will only return saved records from the specified module. |
| `zoho_accounts_url`  | `str` | ZohoAccountsURL.EU | Zoho CRM is hosted at multiple data centers. Therefore, the API domain URL varies for each data center.
You must use your domain-specific Zoho Accounts URL to generate access and refresh tokens. The following are the various domains and their corresponding accounts URLs.
 US: https://accounts.zoho.com
 AU: https://accounts.zoho.com.au
 EU: https://accounts.zoho.eu
 IN: https://accounts.zoho.in
 CN: https://accounts.zoho.com.cn
 JP: https://accounts.zoho.jp
 CA: https://accounts.zohocloud.ca |
| `client_id` :red_circle: | `str` | None | A unique ID displayed under Self Client > Client Secret. |
| `client_secret` :red_circle: | `str` | None | A unique confidential secret displayed under Self Client > Client Secret. |
| `authorization_code` :red_circle: | `str` | None | The authorization code generated during the Self Client creation, used to get the refresh token and the first access token. |
| `refresh_token`  | `str` | None | The refresh token is used to generate a new access token when the current access token expires. |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `sync`  | `bool` | True | When enabled only pushed jobs will remain in the board |
| `update_content`  | `bool` | False | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import ZohoRecruit
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


ZohoRecruit.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        fields="your_fields",
        sort_order="asc",
        sort_by="your_sort_by",
        converted="false",
        approved="true",
        cvid=0,
        territory_id=0,
        include_child=False,
        state="draft",
        zoho_accounts_url="https://accounts.zoho.eu",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        refresh_token="your_refresh_token",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```