
# Push profile

`HrFlow.ai Profiles` :arrow_right: `DigitalRecruiters Profiles`

Pushes a profile from Hrflow.ai to DigitalRecruiters.

**DigitalRecruiters Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Write Profiles**]({digital_recruiters_environment_url}/api/candidate/apply/{token}) | Write profiles to DigitalRecruiters |

## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L162) | Formatting function |
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
| `digital_recruiters_token` :red_circle: | `str` | None | DigitalRecruiters API token. |
| `digital_recruiters_environment_url` :red_circle: | `str` | None | DigitalRecruiters API environment url. |
| `job_reference` :red_circle: | `str` | None | reference of the job to which the candidate is applying. |
| `message`  | `str` | message du candidat | Application message. |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import DigitalRecruiters
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


DigitalRecruiters.ActionName.push_profile(
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
        digital_recruiters_token="your_digital_recruiters_token",
        digital_recruiters_environment_url="your_digital_recruiters_environment_url",
        job_reference="your_job_reference",
        message="message du candidat",
    )
)
```
