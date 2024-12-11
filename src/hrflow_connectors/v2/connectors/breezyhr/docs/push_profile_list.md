# Push profile list
`HrFlow.ai Profiles` :arrow_right: `BreezyHRWarehouse`

Push all profiles from ***Hrflow.ai Source*** via ***BreezyHR*** API and send them to a ***BreezyHR***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L94) | Formatting function |
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
| `email` :red_circle: | `str` | None | email |
| `password` :red_circle: | `str` | None | password |
| `company_id`  | `str` | None | ID of company to pull jobs from in Breezy HR database associated with the authenticated user 
 [⚠️ Requiered if company_name is not specified] |
| `company_name`  | `str` | None | the company associated with the authenticated user 
 [⚠️ Requiered if company_id is not specified] |
| `position_id` :red_circle: | `str` | None | Id of the position to create a new candidate for |
| `origin`  | `str` | sourced | will indicate in Breezy if the candidate should be marked as sourced or applied |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import BreezyHR
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BreezyHR.push_profile_list(
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
        email="your_email",
        password="your_password",
        company_id="your_company_id",
        company_name="your_company_name",
        position_id="your_position_id",
        origin="sourced",
    )
)
```