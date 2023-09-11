
# Pull profile list
`WorkableProfileWarehouse` :arrow_right: `HrFlow.ai Profiles`

Retrieves all profiles via the ***Workable*** API and send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_candidate`](../connector.py#L187) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth` :red_circle: | `str` | None | API KEY |
| `subdomain` :red_circle: | `str` | None | Subdomain |
| `shortcode` :red_circle: | `str` | None | Job shortcode |
| `state`  | `str` | None | The job's stage slug, can be retrieved from the /stages endpoint |
| `limit`  | `int` | None | Specifies the number of candidates to try and retrieve per page |
| `since_id`  | `str` | None | Returns results with an ID greater than or equal to the specified ID. |
| `max_id`  | `str` | None | Returns results with an ID less than or equal to the specified ID. |
| `created_after`  | `<class 'datetime.datetime'>` | None | Returns results created after the specified timestamp. |
| `updated_after`  | `<class 'datetime.datetime'>` | None | Returns results updated after the specified timestamp. |

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
from hrflow_connectors import Workable
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Workable.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        auth="your_auth",
        subdomain="your_subdomain",
        shortcode="your_shortcode",
        state="your_state",
        limit=0,
        since_id="your_since_id",
        max_id="your_max_id",
        created_after=***,
        updated_after=***,
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