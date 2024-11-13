# Push profile list
`HrFlow.ai Profiles` :arrow_right: `Zoho Candidates`

Pushs Profiles from HrFlow to Zoho Recruit



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_hrflow_profile_to_zoho`](../connector.py#L239) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `profile_key` :red_circle: | `str` | None | HrFlow.ai profile key |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
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

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import ZohoRecruit
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


ZohoRecruit.push_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        zoho_accounts_url="https://accounts.zoho.eu",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        refresh_token="your_refresh_token",
    )
)
```