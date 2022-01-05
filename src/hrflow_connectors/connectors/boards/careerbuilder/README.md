# CareerBuilder Connector
**CareerBuilder is an employment website which provides labor market information, talent management software, and other recruitment related services.**

`CareerBuilder`  :arrow_right:  `Hrflow.ai`

## GetAllJobs
`GetAllJobs` scans the ***CareerBuilder*** website for job offers. It adds all these **jobs** to a ***Hrflow.ai Board***

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
| `domain` :red_circle: | `str` | domain just after `careerbuilder.` for example domain =`fr` in `https:/www.careerbuilder.fr`        |
| `job_search` :red_circle: | `str` | Name of the job position we want to search offers for in `https:/www.careerbuilder.fr`        |
| `job_location` :red_circle: | `str` | Location of the job offers        |
| `executable_path`  | `Optional[str]` | A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator        |
| `binary_location`  | `Optional[str]` | Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`        |
| `maximum_page_num`  | `Optional[int]` | Maximum `number of pages` you want to scroll, `career builder`pagination is designed as an infinite scroller loading. Default value : `None` (This means taking the maximum number of pages)        |
| `sort_by_date`  | `bool`  | sort search results by date. Default value: `False` (By default, results are sorted by relevancy)

:red_circle: : *required* 

### Example

```python
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.careerbuilder import GetAllJobs
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

action = GetAllJobs(
    executable_path="/opt/webdriver",
    maximum_page_num=2,
    domain="com",
    hrflow_client=client,
    job_search="Data Scientist",
    job_location="California",
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
action.execute()
```