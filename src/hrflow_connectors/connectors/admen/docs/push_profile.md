# Push profile
`HrFlow.ai Profiles` :arrow_right: `AD-MEN Profiles`

Writes a profile from Hrflow.ai Source to the ***AD-MEN*** database server



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_hrflow_profile_to_admen`](../connector.py#L163) | Formatting function |
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
| `db_host` :red_circle: | `str` | None | The hostname of the database server |
| `db_port` :red_circle: | `int` | None | The port of the database server |
| `db_name` :red_circle: | `str` | None | The name of the database |
| `db_user` :red_circle: | `str` | None | The username to connect to the database |
| `db_password` :red_circle: | `str` | None | The password to connect to the database |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import ADMEN
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


ADMEN.push_profile(
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
        db_host="your_db_host",
        db_port=0,
        db_name="your_db_name",
        db_user="your_db_user",
        db_password="your_db_password",
    )
)
```