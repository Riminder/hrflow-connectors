# Push profile
`HrFlow.ai Profiles` :arrow_right: `TalentSoft Profiles`

Pushs specific Profile from HrFlow and writes it to Applicant object in Talentsoft



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_info_ts_applicant`](../connector.py#L378) | Formatting function |
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
| `client_id` :red_circle: | `str` | None | client id used to access TalentSoft front office API |
| `client_secret` :red_circle: | `str` | None | client secret used to access TalentSoft front office API |
| `client_url` :red_circle: | `str` | None | url used to access TalentSoft front office API |
| `job_reference`  | `str` | None | reference of the job offer to which the candidate is applying |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import TalentSoft
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


TalentSoft.push_profile(
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
        client_id="your_client_id",
        client_secret="your_client_secret",
        client_url="your_client_url",
        job_reference="your_job_reference",
    )
)
```