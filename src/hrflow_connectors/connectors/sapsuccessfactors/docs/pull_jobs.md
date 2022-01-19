# Pull jobs

`SAP(SuccessFactors)` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs from SAPSuccessFactors via their ***Job Requisition API***. It adds all these **jobs** to a ***Hrflow.ai Board***.


| Endpoints | Description |
| --------- | ----------- |
| [Get Job Requisitions](https://api.sap.com/api/RCMJobRequisition/overview)| Endpoint to access job requisition data, an api_server is required, the request method is `GET` |

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
| `auth` :red_circle: | `Union[XAPIKeyAuth, OAuth2PasswordCredentialsBody]` | Auth instance to identify and communicate with the platform        |
| `api_server` :red_circle: | `str` | api_server: the `api_server` in `https://{api-server}/odata/v2`. For example api_server=`apisalesdemo8.successfactors.com` in `https://apisalesdemo8.successfactors.com/odata/v2`        |
| `top`  | `int` | show only the first n items, value by default = `20`

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import SAPSuccessfactors
from hrflow import Hrflow
from hrflow_connectors import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

auth = XAPIKeyAuth(name = "APIKey", value = "MY_API_KEY")

SapSuccessfactors.pull_jobs(
    auth=auth,
    top = 30,
    api_server="MY_API_SERVER",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
    )
```