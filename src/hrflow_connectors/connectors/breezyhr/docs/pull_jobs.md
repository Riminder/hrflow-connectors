# Pull jobs
`Breezyhr` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all your company available jobs in Breezy.hr via their ***Breezyhr API***. It adds all these **jobs** to a ***Hrflow.ai Board***.

🔗 [Documentation](https://developer.breezy.hr/docs/company-positions)

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
| `auth` :red_circle: | `OAuth2EmailPasswordBody` | Auth instance to identify and communicate with the platform        |
| `company_name` | `Optional[str]` | Name of the company associated with the authenticated user, required if you haven't specified your company id. Default value `None`       |
| `company_id` | `Optional[str]` | Id of the company associated with the authenticated user, Default value `None`      |

:red_circle: : *required*

## Example

```python
from hrflow_connectors import Breezyhr

from hrflow import Hrflow
from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

auth = OAuth2EmailPasswordBody(
            access_token_url="https://api.breezy.hr/v3/signin",
            email = settings["EMAIL"]
            password=settings["PASSWORD"],
        )


Breezyhr.pull_jobs(
    auth=auth,
    subdomain="MY_SUBDOMAIN",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    company_name=settings["MY_COMPANY_NAME"],
    hydrate_with_parsing=True,
)
```