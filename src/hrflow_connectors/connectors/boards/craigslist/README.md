# Craigslist Connector
**Craigslist is a website listing ads.**

`Craigslist` :arrow_right: `Hrflow.ai`

## CraigslistFeed
`CraigslistFeed` scans the ***Craigslist*** site for job offers. It adds all these **jobs** to a ***Hrflow.ai Board***.

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
| `subdomain` :red_circle: | `str` | Subdomain just before `craigslist.org/d/emploi/search/jjj` for example subdomain = `paris` in `https://paris.craigslist.org/d/emploi/search/jjj`, it is also the localisation of the job offers        |
| `executable_path`  | `Optional[str]` | A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator        |
| `binary_location`  | `Optional[str]` | Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`        |

:red_circle: : *required* 

### Example

```python
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.craigslist import CraigslistFeed
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

action = CraigslistFeed(
    executable_path="/opt/webdriver",
    subdomain="Paris",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
action.execute()
```