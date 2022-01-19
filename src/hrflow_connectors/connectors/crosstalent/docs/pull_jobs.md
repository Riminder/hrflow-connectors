# Pull jobs
`Crosstalent` :arrow_right: `Hrflow.ai`

`PullJobsAction` gets all available jobs via their ***Salesforce API***. It adds all these **jobs** to a ***Hrflow.ai Board***.

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
| `auth` :red_circle: | `OAuth2PasswordCredentialsBody` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`        |

:red_circle: : *required*

## Example

```python
from hrflow_connectors import Crosstalent
from hrflow import Hrflow
from hrflow_connectors import OAuth2PasswordCredentialsBody

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
auth = OAuth2PasswordCredentialsBody(
    access_token_url="https://test.salesforce.com/services/oauth2/token",
    client_id="MY_CLIENT_ID",
    client_secret="MY_CLIENT_SECRET",
    username="MY_USERNAME",
    password="MY_PASSWORD",
)

Crosstalent.pull_jobs(
    auth=auth,
    subdomain="MY_SUBDOMAIN",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```