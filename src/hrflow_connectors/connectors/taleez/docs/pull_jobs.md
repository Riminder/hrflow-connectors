# Pull jobs
`Taleez` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on a ***Taleez endpoint***. It adds all these **jobs** to a ***Hrflow.ai Board***.

# Links to Taleez documentation on the endpoints used :

| Endpoints | Description |
| --------- | ----------- |
|[ List all jobs](https://api.taleez.com/swagger-ui/index.html?configUrl=/openapi.json/swagger-config#/jobs/list_2) | Endpoint to list all jobs in your company, the request method is `GET` |

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
| `Auth` | :red_circle: | `XTaleezAuth` | Required to access Taleez API.
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |
| `page` :red_circle: | `int` | page number, starts at 0, value by default is 0     |
| `page_size` :red_circle: | `int` | Page size. Max size of the list returned. Max value : 100, default value is 100|


:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Taleez
from hrflow import Hrflow
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

auth = XTaleezAuth(value='MY_X_TALEEZ_API_KEY')

Taleez.pull_jobs(
    page=MY_START_PAGE,
    page_size=MY_PAGE_SIZE_LIMIT,
    auth=auth,
    hrflow_client=client,,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```