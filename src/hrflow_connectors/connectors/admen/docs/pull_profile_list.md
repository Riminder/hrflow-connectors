# Pull profile list
`AD-MEN Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves profiles from the ***AD-MEN*** database serverand send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_hrflow_profile_to_admen`](../connector.py#L128) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `db_host` :red_circle: | `str` | None | The hostname of the database server |
| `db_port` :red_circle: | `int` | None | The port of the database server |
| `db_name` :red_circle: | `str` | None | The name of the database |
| `db_user` :red_circle: | `str` | None | The username to connect to the database |
| `db_password` :red_circle: | `str` | None | The password to connect to the database |
| `limit`  | `int` | 100 | The number of profiles to retrieve |
| `offset`  | `int` | 0 | The offset to start retrieving profiles |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `edit`  | `bool` | False | When enabled the profile must exist in the source |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Admen
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Admen.pull_profile_list(
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
        limit=100,
        offset=0,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        edit=False,
        only_edit_fields=***,
    )
)
```