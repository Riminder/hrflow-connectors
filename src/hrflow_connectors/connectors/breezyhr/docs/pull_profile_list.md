# Pull profile list
`BreezyHRWarehouse` :arrow_right: `HrFlow.ai Profiles`

Retrieves all profiles via the ***BreezyHR*** API and send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_candidate`](../connector.py#L208) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `str` | None | email |
| `password` :red_circle: | `str` | None | password |
| `company_id`  | `str` | None | ID of company to pull jobs from in Breezy HR database associated with the authenticated user |
| `company_name`  | `str` | None | [⚠️ Requiered if company_id is not specified], the company associated with the authenticated user |
| `position_id` :red_circle: | `str` | None | Id of the position to create a new candidate for |

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
from hrflow_connectors import BreezyHR
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BreezyHR.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        email="your_email",
        password="your_password",
        company_id="your_company_id",
        company_name="your_company_name",
        position_id="your_position_id",
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