
# Push profile
`HrFlow.ai Profiles` :arrow_right: `SmartRecruiters Profiles`

Writes a profile from Hrflow.ai Source to SmartRecruiters via the API for the given `job_id`.



**SmartRecruiters Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Post Candidate**](https://dev.smartrecruiters.com/customer-api/live-docs/candidate-api/) | Endpoint to create a new candidate and assign to a talent pool, the request method is `POST` |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L156) | Formatting function |

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
| `x_smart_token` :red_circle: | `str` | None | X-SmartToken used to access SmartRecruiters API |
| `job_id` :red_circle: | `str` | None | Id of a Job to which you want to assign a candidates when it’s created. Profiles are sent to this URL `https://api.smartrecruiters.com/jobs/{job_id}/candidates`  |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import SmartRecruiters


logging.basicConfig(level=logging.INFO)


SmartRecruiters.push_profile(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        x_smart_token="your_x_smart_token",
        job_id="your_job_id",
    )
)
```