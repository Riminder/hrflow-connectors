
# Pull jobs
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
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L96) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `str` | None | X-SmartToken used to access SmartRecruiters API |
| `query`  | `str` | None | Case insensitive full-text query against job title e.g. java developer |
| `updated_after`  | `str` | None | ISO8601-formatted time boundaries for the job update time |
| `posting_status`  | `str` | None | Posting status of a job. One of ['PUBLIC', 'INTERNAL', 'NOT_PUBLISHED', 'PRIVATE'] |
| `job_status`  | `str` | None | Status of a job. One of ['CREATED', 'SOURCING', 'FILLED', 'INTERVIEW', 'OFFER', 'CANCELLED', 'ON_HOLD'] |

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


logging.basicConfig(level=logging.INFO)


SmartRecruiters.pull_jobs(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    source_parameters=dict(
        x_smart_token="your_x_smart_token",
        query="your_query",
        updated_after="your_updated_after",
        posting_status="PUBLIC",
        job_status="CREATED",
    ),
    destination_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```