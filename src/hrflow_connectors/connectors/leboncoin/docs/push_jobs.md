
# Push jobs
`HrFlow.ai Jobs` :arrow_right: `LeboncoinWarehouse`

Retrieves all jobs from an HrFlow JobBoard and sends them through the Leboncoin API



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_ad`](../connector.py#L86) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board keys to extract the job from |
| `job_key` :red_circle: | `str` | None |  |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `secret_key` :red_circle: | `str` | None | Leboncoin API secret key |
| `http_auth_identity` :red_circle: | `str` | None | Client's name |
| `morpheus_client_id` :red_circle: | `int` | None | Client's unique ID |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Leboncoin


logging.basicConfig(level=logging.INFO)


Leboncoin.push_jobs(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        job_key="your_job_key",
    ),
    target_parameters=dict(
        secret_key="your_secret_key",
        http_auth_identity="your_http_auth_identity",
        morpheus_client_id=0,
    )
)
```