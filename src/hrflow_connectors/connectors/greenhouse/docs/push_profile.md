# Push profile
`HrFlow.ai Profiles` :arrow_right: `Greenhouse Profiles`

Writes a profile from Hrflow.ai Source to Greenhouse  via the API for the given job_id(s) provided in tags.



**Greenhouse Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Post Candidate**](https://developers.greenhouse.io/job-board.html#jobs) | Endpoint to create a new candidate and assign to a talent pool, the request method is `POST` |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L92) | Formatting function |
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
| `auth` :red_circle: | `str` | None | XAPIKeyAuth |
| `on_behalf_of` :red_circle: | `str` | None | The ID of the user sending the profile, or the person he is sending the profile on behalf of |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Greenhouse
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Greenhouse.push_profile(
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
        auth="your_auth",
        on_behalf_of="your_on_behalf_of",
    )
)
```