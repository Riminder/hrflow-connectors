# Pull job list
`Adzuna Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs via the ***Adzuna'*** API Search endpointand send them to a ***Hrflow.ai Board***.


**Adzuna Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get Adzuna jobs**](https://api.adzuna.com/v1/doc/Search.md) | Use this endpoint to retrieve Adzuna's job advertisement listings. |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L42) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `country` :red_circle: | `str` | None | ISO 8601 country code of the country of interest |
| `app_id` :red_circle: | `str` | None | Application ID, supplied by Adzuna |
| `app_key` :red_circle: | `str` | None | Application key, supplied by Adzuna |
| `results_per_page`  | `int` | None | The number of results to include on a page of search results. |
| `what`  | `str` | None | The keywords to search for. Multiple terms may be space separated. |
| `what_and`  | `str` | None | The keywords to search for, all keywords must be found. |
| `what_phrase`  | `str` | None | An entire phrase which must be found in the description or title. |
| `what_or`  | `str` | None | The keywords to search for, any keywords may be found. Multiple terms may be space separated. |
| `what_exclude`  | `str` | None | Keywords to exclude from the search. Multiple terms may be space separated. |
| `title_only`  | `str` | None | Keywords to find, but only in the title. Multiple terms may be space separated. |
| `where`  | `str` | None | The geographic centre of the search. Place names, postal codes, etc. may be used.	 |
| `distance`  | `int` | None | The distance in kilometres from the centre of the place described by the 'where' parameter. Defaults to 5km. |
| `location0`  | `str` | None | The location fields may be used to describe a location, in a similar form to that returned in a Adzuna::API::Response::Location object.For example, "location0=UK&location1=South East England&location2=Surrey"  will performn a search over the county of Surrey. |
| `location1`  | `str` | None |  |
| `location2`  | `str` | None |  |
| `location3`  | `str` | None |  |
| `location4`  | `str` | None |  |
| `location5`  | `str` | None |  |
| `location6`  | `str` | None |  |
| `location7`  | `str` | None |  |
| `max_days_old`  | `int` | None | The age of the oldest advertisment in days that will be returned. |
| `category`  | `str` | None | The category tag, as returned by the "category" endpoint. |
| `sort_dir`  | `str` | None | The direction to order the search results. |
| `sort_by`  | `str` | None | The ordering of the search results. |
| `salary_min`  | `int` | None | The minimum salary we wish to get results for. |
| `salary_max`  | `int` | None | The maximum salary we wish to get results for. |
| `salary_include_unknown`  | `str` | None | If set it "1", jobs without a known salary are returned. |
| `full_time`  | `str` | None | If set to "1", only full time jobs will be returned. |
| `part_time`  | `str` | None | If set to "1", only part time jobs will be returned. |
| `contract`  | `str` | None | If set to "1", only contract jobs will be returned. |
| `permanent`  | `str` | None | If set to "1", only permanent jobs will be returned. |
| `company`  | `str` | None | The canonical company name. This may be returned in a Adzuna::API::Response::Company object when a job is returned. A full list of allowed terms in not available through the API. |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `sync`  | `bool` | True | When enabled only pushed jobs will remain in the board |
| `update_content`  | `bool` | False | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Adzuna
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Adzuna.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        country="gb",
        app_id="your_app_id",
        app_key="your_app_key",
        results_per_page=0,
        what="your_what",
        what_and="your_what_and",
        what_phrase="your_what_phrase",
        what_or="your_what_or",
        what_exclude="your_what_exclude",
        title_only="your_title_only",
        where="your_where",
        distance=0,
        location0="your_location0",
        location1="your_location1",
        location2="your_location2",
        location3="your_location3",
        location4="your_location4",
        location5="your_location5",
        location6="your_location6",
        location7="your_location7",
        max_days_old=0,
        category="your_category",
        sort_dir="up",
        sort_by="default",
        salary_min=0,
        salary_max=0,
        salary_include_unknown="1",
        full_time="1",
        part_time="1",
        contract="1",
        permanent="1",
        company="your_company",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```