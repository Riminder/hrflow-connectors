# Pull jobs
`Greenhouse` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs listed on ***Greenhouse board***. It adds all these **jobs** to a ***Hrflow.ai Board***.


Links to Greenhouse documentation on the endpoints used :
| Endpoints | Description |
| --------- | ----------- |
| [Job Board](https://developers.greenhouse.io/job-board.html) | Endpoint to retrieve all jobs from a greenhouse job board which is publicly available, only required parameter is a board token or board name, the request method is `GET`|

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
| `board_token` :red_circle: | `str` |  Job Board data in `Greenhouse` is publicly available, so authentication is not required for any GET endpoints. `board_token` is the identifier of a given board on greenhouse, for example `lyft` for the `Lyft Company`, for testing use board_token = `vaulttec`. It is inserted in the url: "https://boards-api.greenhouse.io/v1/boards/{board_token}/jobs/?content=true"     |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Greenhouse
from hrflow import Hrflow
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

Greenhouse.pull_jobs(
    hrflow_client=client,
    board_token="MY_BOARD_TOKEN",
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```