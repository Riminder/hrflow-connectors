
# Pull profile list
`Taleez Profiles Warehouse` :arrow_right: `HrFlow.ai Profile Parsing`

Retrieves all profiles via the ***Taleez*** API and send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_taleez_candidate`](../connector.py#L352) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_taleez_api_secret` :red_circle: | `str` | None | X-taleez-api-secret used to access Taleez API |
| `mail`  | `str` | None | Filter by mail |

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
from hrflow_connectors import Taleez
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Taleez.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        x_taleez_api_secret="your_x_taleez_api_secret",
        mail="your_mail",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        only_insert=False,
    )
)
```