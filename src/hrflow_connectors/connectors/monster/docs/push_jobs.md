
# Push jobs
`HrFlow.ai Jobs` :arrow_right: `Monster Jobs`

push a job from  ***Hrflow*** to ***Monster*** API. To see job pushed, go to http://jobview.monster.com/getjob.aspx?jobid=xxx with xxx is jobposting id, findable in response. Manual Test for push here: https://integrations.monster.com/Toolkit/



**Monster Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get job**](https://partner.monster.com/real-time-posting-devguide) | Endpoint to push the content of a job with a given job key and board key the request method is `POST` |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L164) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `job_key` :red_circle: | `str` | None | HrFlow.ai job key |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `username` :red_circle: | `str` | None | Monster username |
| `password` :red_circle: | `str` | None | Monster password |
| `api_key` :red_circle: | `str` | None | API key to submit |
| `subdomain` :red_circle: | `str` | None | Subdomain monster just before `monster.com`. For example subdomain=`my_subdomain.my` in `https//my_subdomain.my.monster.com8443/bgwBroker |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Monster
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Monster.push_jobs(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        job_key="your_job_key",
    ),
    target_parameters=dict(
        username="your_username",
        password="your_password",
        api_key="your_api_key",
        subdomain="your_subdomain",
    )
)
```