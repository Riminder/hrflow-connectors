# Push profile
`HrFlow.ai Profiles` :arrow_right: `Salesforce Profiles`

Pushs specific Profile from HrFlow and writes it to HrFlow_Profile__c & Co Custom Objects in Salesforce



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_into_salesforce_profile`](../connector.py#L124) | Formatting function |
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
| `sf_username` :red_circle: | `str` | None | username used to access Salesforce API |
| `sf_password` :red_circle: | `str` | None | password used to access Salesforce API |
| `sf_security_token` :red_circle: | `str` | None | Security Token to access Salesforce API.See below for instructions: How Can I Find My Security Token and Use It in Data Loader | Salesforce Platform  https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport |
| `sf_organization_id` :red_circle: | `str` | None | Security Token to access Salesforce API.See below for instructions: How to find your organization id  https://help.salesforce.com/s/articleView?id=000385215&type=1 |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Salesforce
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Salesforce.push_profile(
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
        sf_username="your_sf_username",
        sf_password="your_sf_password",
        sf_security_token="your_sf_security_token",
        sf_organization_id="your_sf_organization_id",
    )
)
```