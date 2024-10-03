# Pull job list
`Taleez Jobs Warehouse` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***Taleez*** API and send them to a ***Hrflow.ai Board***.


**Taleez Jobs Warehouse endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all jobs**](https://api.taleez.com/0/jobs) | Endpoint to retrieve all jobs. and get the list of all jobs with their ids, the request method is `GET` |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L337) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_taleez_api_secret` :red_circle: | `str` | None | X-taleez-api-secret used to access Taleez API |
| `with_details` :red_circle: | `bool` | None | xxx |
| `job_status`  | `str` | None | Posting status of a job. One of ['PUBLISHED'] |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `sync`  | `bool` | True | When enabled only pushed jobs will remain in the board |
| `update_content`  | `bool` | False | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Taleez
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Taleez.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        x_taleez_api_secret="your_x_taleez_api_secret",
        with_details=False,
        job_status="PUBLISHED",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```