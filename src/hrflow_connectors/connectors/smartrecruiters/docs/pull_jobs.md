# Pull jobs
`SmartRecruiters` :arrow_right: `Hrflow.ai`

`PullJobsAction` retrieves all jobs via the ***SmartRecruiter*** API. It adds all these **jobs** to a ***Hrflow.ai Board***.

**Links to SmartRecruiters documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
|[Get all jobs](https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.all) | Endpoint to search jobs by traditional params (offset, limit...) and get the list of all jobs with their ids, the request method is `GET`|
|[Get job content](https://dev.smartrecruiters.com/customer-api/live-docs/job-api/#/jobs/jobs.get) | Endpoint to get the content of a job with a given id, a jobId parameter is required, the request method is `GET`|

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |
| `auth` :red_circle: | `XSmartTokenAuth` | Auth instance to identify and communicate with the platform        |
| `query` | `Optional[str]` | Full-text search query based on a job title; case insensitive; e.g. java developer. Default value : `None`        |
| `updated_after` | `Optional[str]` | Posting status of a job. Available values : PUBLIC, INTERNAL, NOT_PUBLISHED, PRIVATE. Default value : `None`        |
| `posting_status` | `Optional[str]` | Posting status of a job. Available values : PUBLIC, INTERNAL, NOT_PUBLISHED, PRIVATE. Default value : `None`        |
| `job_status` | `Optional[str]` | Status of a job. Available values : CREATED, SOURCING, FILLED, INTERVIEW, OFFER, CANCELLED, ON_HOLD. Default value : `None`        |
| `limit` | `int` | Number of elements to return per page. max value is 100. Default value : `10`        |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import SmartRecruiter
from hrflow import Hrflow
from hrflow_connectors import XSmartTokenAuth

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
auth = XSmartTokenAuth(value=smart_token)

SmartRecruiter.pull_jobs(
    auth=auth,
    hrflow_client=client,
    limit=2,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=False,
    archive_deleted_jobs_from_stream=False,
    posting_status="PUBLIC",
)
```