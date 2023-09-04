# Pull job list
`Salesforce Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Salesforce HrFlow Job Custom Object and writes them to an Hrflow.ai board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L237) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `sf_username` :red_circle: | `str` | None | username used to access Salesforce API |
| `sf_password` :red_circle: | `str` | None | password used to access Salesforce API |
| `sf_security_token` :red_circle: | `str` | None | Security Token to access Salesforce API.See below for instructions: How Can I Find My Security Token and Use It in Data Loader | Salesforce Platform  https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport |
| `sf_organization_id` :red_circle: | `str` | None | Security Token to access Salesforce API.See below for instructions: How to find your organization id  https://help.salesforce.com/s/articleView?id=000385215&type=1 |
| `last_modified_date`  | `str` | None | Last modified date |
| `limit`  | `int` | 500 | Number of items to pull from Salesforce. Maximum value is 500 |

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
from hrflow_connectors import Salesforce
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Salesforce.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        sf_username="your_sf_username",
        sf_password="your_sf_password",
        sf_security_token="your_sf_security_token",
        sf_organization_id="your_sf_organization_id",
        last_modified_date="your_last_modified_date",
        limit=500,
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