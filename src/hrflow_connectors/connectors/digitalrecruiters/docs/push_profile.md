# Push profile
`HrFlow.ai Profiles` :arrow_right: `DigitalRecruiters Write Profile`

Pushes a profile from Hrflow.ai to Digital Recruiters.



**DigitalRecruiters Write Profile endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Write Profile**]({url_environnement}/api/candidate/apply/{token}) | Write profile to Digital Recruiters |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L284) | Formatting function |
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
| `token` :red_circle: | `str` | None | Digital Recruiters API token. |
| `environment_url` :red_circle: | `str` | None | Digital Recruiters API url environnement. |
| `job_reference` :red_circle: | `str` | None | reference of the job to which the candidate is applying. |
| `message`  | `str` | message du candidat | Application message. |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import DigitalRecruiters
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


DigitalRecruiters.push_profile(
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
        token="your_token",
        environment_url="your_environment_url",
        job_reference="your_job_reference",
        message="message du candidat",
    )
)
```