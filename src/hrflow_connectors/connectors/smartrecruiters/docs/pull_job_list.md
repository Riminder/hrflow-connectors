# Pull job list
`SmartRecruiters Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***SmartRecruiter*** API and send them to a ***Hrflow.ai Board***.


**SmartRecruiters Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all jobs**](https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.all) | Endpoint to search jobs by traditional params (offset, limit...) and get the list of all jobs with their ids, the request method is `GET` |
| [**Get job**](https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.get) | Endpoint to get the content of a job with a given id, a jobId parameter is required, the request method is `GET` |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L99) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `str` | None | X-SmartToken used to access SmartRecruiters API |
| `query`  | `str` | None | Case insensitive full-text query against job title e.g. java developer |
| `updated_after`  | `str` | None | ISO8601-formatted time boundaries for the job update time |
| `posting_status`  | `str` | None | Posting status of a job. One of ['PUBLIC', 'INTERNAL', 'NOT_PUBLISHED', 'PRIVATE'] |
| `job_status`  | `str` | None | Status of a job. One of ['CREATED', 'SOURCING', 'FILLED', 'INTERVIEW', 'OFFER', 'CANCELLED', 'ON_HOLD'] |
| `limit`  | `int` | 100 | Number of items to pull from SmartRecruiters at a time. Not matter what value is supplied it is capped at 100 |

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
from hrflow_connectors import SmartRecruiters
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


SmartRecruiters.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        x_smart_token="your_x_smart_token",
        query="your_query",
        updated_after="your_updated_after",
        posting_status="PUBLIC",
        job_status="CREATED",
        limit=100,
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