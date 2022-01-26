# Pull jobs

`Teamtailor` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on ***Teamtailor company endpoints***. It adds all these **jobs** to a ***Hrflow.ai Board***.

**Links to Teamtailor documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [List Jobs](https://docs.teamtailor.com/#a0351972-8394-4646-95f0-56a4c4e3886d) | Endpoint to retrieve a list of your company jobs, the request method is `GET`|

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `auth` :red_circle: | `AuthorizationAuth`| Auth instance to identify and communicate with the platform        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Teamtailor
from hrflow_connectors import AuthorizationAuth
from hrflow import Hrflow

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
auth = AuthorizationAuth(
    name = 'Authorization',
    value= 'MY_AUTHORIZATION',
)

action = Teamtailor.PullJobs(
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```