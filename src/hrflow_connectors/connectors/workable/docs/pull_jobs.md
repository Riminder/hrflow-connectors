# Pull jobs

`Workable` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on ***Workable public endpoints***. It adds all these **jobs** to a ***Hrflow.ai Board***.

**Links to Workable documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [subdomain](https://workable.readme.io/docs/jobs-1) | Endpoint to get a collection of the public jobs for an account subdomain, the request method is `GET` |

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
| `subdomain` :red_circle: | `str` | Companies have jobs listed on workable public endpoints, subdomain of a company endpoint in `https://www.workable.com/api/accounts/{subdomain}` for example subdomain=`eurostar` for ***Eurostar*** company      |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Workable
from hrflow import Hrflow

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

Workable.pull_jobs(
    subdomain="MY_SUBDOMAIN",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```