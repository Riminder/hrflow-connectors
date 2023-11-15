# Pull profile list
`Ceipal Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves profiles from Ceipal and writes them to an Hrflow.ai source



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L100) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `str` | None | Email of the user to authenticate |
| `password` :red_circle: | `str` | None | Password of the user to authenticate |
| `api_key` :red_circle: | `str` | None | API key of the user to authenticate |
| `limit`  | `int` | 20 | Default page limit is 20 and can go up to 50 records per page. |
| `created_by`  | `int` | None | User ID of the applicant creator |
| `source`  | `int` | None | Source ID of the applicants |
| `applicant_status`  | `int` | None | Applicant status ID |
| `sortorder`  | `str` | None | Sort order (asc or desc) |
| `sortby`  | `str` | None | Sort by field (e.g., applicant_id) |

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
from hrflow_connectors import Ceipal
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Ceipal.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        email="your_email",
        password="your_password",
        api_key="your_api_key",
        limit=20,
        created_by=0,
        source=0,
        applicant_status=0,
        sortorder="your_sortorder",
        sortby="your_sortby",
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