# Indeed Connector
**Indeed is a meta job search engine. It provides access to millions of job offers.**

`Indeed` :arrow_right: `Hrflow.ai`

## IndeedFeed
`IndeedFeed` scans the ***Indeed*** site for job offers. It adds all these **jobs** to a ***Hrflow.ai Board***.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `False`        |
| `subdomain` :red_circle: | `str` | Subdomain just before `indeed.com` for example subdomain =`fr` in `https:/fr.indeed.com`        |
| `job_search` :red_circle: | `str` | Name of the job position we want to search offers in `fr.indeed.com`        |
| `job_location` :red_circle: | `str` | Location of the job offers        |
| `executable_path`  | `Optional[str]` | A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator        |
| `binary_location`  | `Optional[str]` | Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`        |
| `max_page`  | `Optional[int]` | Maximum number of pages to search. Default value : `None` (This means taking the maximum number of pages)        |

:red_circle: : *required* 

### Example

```python
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.indeed import IndeedFeed
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

action = IndeedFeed(
    executable_path="/opt/webdriver",
    max_page=2,
    subdomain="fr",
    hrflow_client=client,
    job_search="Software Engineer",
    job_location="Paris",
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
action.execute()
```