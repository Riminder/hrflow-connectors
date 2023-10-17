# Pull job list
`Ceipal Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Ceipal and writes them to an Hrflow.ai board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L21) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `email` :red_circle: | `str` | None | Email of the user to authenticate |
| `password` :red_circle: | `str` | None | Password of the user to authenticate |
| `api_key` :red_circle: | `str` | None | API key of the user to authenticate |
| `limit`  | `int` | 20 | Default page limit is 20 and can go up to 50 records per page. |
| `business_unit_id`  | `int` | None | Every job is associated with a business unit. Pass the business unit id as the parameter to get the jobs. |
| `country`  | `int` | None | Pull the jobs based on the country. Use the countries endpoint to get the list of countries. |
| `state`  | `int` | None | Pass the state id as the parameter to get the jobs. Use the states endpoint to get the states list. |
| `job_status`  | `int` | None | Use the job status endpoint from the master data to get the job statuses. Pass the id here to pull the matching jobs. |
| `post_on_careerportal`  | `bool` | None | Send 1 to get all the jobs that are posted on the careers page. 0 for the jobs that are not posted. |
| `fromdate`  | `str` | None | To get the jobs in between the dates, use this parameter (date format: mm-dd-yyyy) |
| `todate`  | `str` | None | This parameter works along with the fromdate. Gives the jobs that are created between the from date and to date (date format: mm-dd-yyyy) |
| `filter`  | `str` | None | When the from date and to dates are used, this parameter is mandatory. Use ‘created’ as the filter value to get the jobs that are created between the from and to dates. |
| `posted_ago_days`  | `int` | None | Pass any numeric value to get the jobs that are created within this number of days. |
| `sortorder`  | `str` | None | Use either asc or desc to sort the job postings list. |
| `sortby`  | `str` | None | This filter is used along with the sortorder. Job postings can be sorted based on the job code, created date, modified date. |
| `assigned_recruiter`  | `<class 'dict'>` | None | Pass the assigned recruiter as a dictionary. |

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
from hrflow_connectors import Ceipal
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Ceipal.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        email="your_email",
        password="your_password",
        api_key="your_api_key",
        limit=20,
        business_unit_id=0,
        country=0,
        state=0,
        job_status=0,
        post_on_careerportal=False,
        fromdate="your_fromdate",
        todate="your_todate",
        filter="your_filter",
        posted_ago_days=0,
        sortorder="your_sortorder",
        sortby="your_sortby",
        assigned_recruiter=***,
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