# Pull jobs

`Recruitee` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on ***Recruitee company endpoints***. It adds all these **jobs** to a ***Hrflow.ai Board***.

**Links to Recruitee documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Get offers](https://docs.recruitee.com/reference/offers) | Endpoint that returns a collection of published company jobs, a company subdomain parameter is required, the request method is `GET`|

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
| `subdomain` :red_circle: | `str` | subdomain of your company endpoint or the company you want to pull jobs from in `https://{subdomain}.recruitee.com/api/offers` for example subdomain=`testhr` for ***TESTHR*** an example company created to test     |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Recruitee
from hrflow import Hrflow
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

action = Recruitee.PullJobs(
    subdomain="MY_SUBDOMAIN",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```