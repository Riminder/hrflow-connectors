
# Push profile
`HrFlow.ai Profiles` :arrow_right: `SmartRecruiters Profiles`

Writes a profile from Hrflow.ai Source to Broadbean via the API



**SmartRecruiters Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Send candidate**](https://integrations.broadbean.com/hc/en-us/articles/115004599865-Sending-a-Candidate-in-) | This route accepts a POST request consisting of a JSON payload and some authentication Headers. The response will consist of a success flag and either a transaction ID (success) or an error. |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile_push`](../connector.py#L46) | Formatting function |
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
| `secret_key` :red_circle: | `str` | None | secret provided by Broadbean |
| `source_id` :red_circle: | `str` | None | Candidate Hub source ID |
| `job_id` :red_circle: | `str` | None | The ID of the Job the candidate has applied to |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Broadbean
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Broadbean.push_profile(
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
        secret_key="your_secret_key",
        source_id="your_source_id",
        job_id="your_job_id",
    )
)
```