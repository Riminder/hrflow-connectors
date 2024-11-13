# Pull profile list
`AD-MEN Profiles` :arrow_right: `HrFlow.ai Profile Parsing`

Retrieves profiles from the ***AD-MEN*** database serverand send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_admen_profile_to_hrflow`](../connector.py#L285) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `db_host` :red_circle: | `str` | None | The hostname of the database server |
| `db_port` :red_circle: | `int` | None | The port of the database server |
| `db_name` :red_circle: | `str` | None | The name of the database |
| `db_user` :red_circle: | `str` | None | The username to connect to the database |
| `db_password` :red_circle: | `str` | None | The password to connect to the database |
| `share_server` :red_circle: | `str` | None | The hostname of the network share server |
| `share_name` :red_circle: | `str` | None | The name of the network share |
| `share_username` :red_circle: | `str` | None | The username to connect to the network share |
| `share_password` :red_circle: | `str` | None | The password to connect to the network share |
| `share_domain` :red_circle: | `str` | None | The domain to connect to the network share |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `only_insert`  | `bool` | False | When enabled the profile is written only if it doesn't exist in the source |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import ADMEN
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


ADMEN.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        db_host="your_db_host",
        db_port=0,
        db_name="your_db_name",
        db_user="your_db_user",
        db_password="your_db_password",
        share_server="your_share_server",
        share_name="your_share_name",
        share_username="your_share_username",
        share_password="your_share_password",
        share_domain="your_share_domain",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        only_insert=False,
    )
)
```