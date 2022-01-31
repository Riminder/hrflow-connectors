# Pull jobs

`Ceridian Dayforce` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on ***Ceridian Dayforce specific endpoints***. It adds all these **jobs** to a ***Hrflow.ai Board***.

**Links to Ceridian Dayforce documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
|[Get Job Feeds](https://developers.dayforce.com/Build/API-Explorer/Recruiting/Get-Job-Postings.aspx)         | Endpoint to get job postings that have been posted externally and are available through the candidate portal, a clientNamespace parameter is required, the reauest method is `Get`            |
## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter . Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |
| `subdomain` :red_circle: | `str` | subdomain just before `dayforcehcm.com`     |
| `client_name_space` :red_circle: | `str` | Uniquely identifies the client's Dayforce instance for example. Is needed to login |
:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Ceridian
from hrflow import Hrflow

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

Ceridian.pull_jobs(
    subdomain="MY_SUBDOMAIN",
    client_name_space="MY_CLIENT_NAME_SPACE"
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```