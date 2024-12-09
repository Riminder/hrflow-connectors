# Update jobs in hrflow
`Adzuna` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Adzuna _to_ HrFlow


**Adzuna endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Get Adzuna jobs**](https://api.adzuna.com/v1/doc/Search.md) | Use this endpoint to retrieve Adzuna's job advertisement listings. |



## Adzuna Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `app_id` :red_circle: | `string\|null` | None | Application ID, supplied by Adzuna |
| `app_key` :red_circle: | `string\|null` | None | Application key, supplied by Adzuna |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Adzuna)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `country` :red_circle: | `Literal['at','au','br','ca','de','fr','gb','in','it','nl','nz','pl','ru','sg','us','za']` | None | ISO 8601 country code of the country of interest |
| `results_per_page`  | `integer\|null` | 50 | The number of results to include on a page of search results. |
| `what`  | `string\|null` | None | The keywords to search for. Multiple terms may be space separated. |
| `what_and`  | `string\|null` | None | The keywords to search for, all keywords must be found. |
| `what_phrase`  | `string\|null` | None | An entire phrase which must be found in the description or title. |
| `what_or`  | `string\|null` | None | The keywords to search for, any keywords may be found. Multiple terms may be space separated. |
| `what_exclude`  | `string\|null` | None | Keywords to exclude from the search. Multiple terms may be space separated. |
| `title_only`  | `string\|null` | None | Keywords to find, but only in the title. Multiple terms may be space separated. |
| `where`  | `string\|null` | None | The geographic centre of the search. Place names, postal codes, etc. may be used.	 |
| `distance`  | `integer\|null` | None | The distance in kilometres from the centre of the place described by the 'where' parameter. Defaults to 5km. |
| `location0`  | `string\|null` | None | The location fields may be used to describe a location, in a similar form to that returned in a Adzuna::API::Response::Location object.For example, "location0=UK&location1=South East England&location2=Surrey"  will performn a search over the county of Surrey. |
| `location1`  | `string\|null` | None |  |
| `location2`  | `string\|null` | None |  |
| `location3`  | `string\|null` | None |  |
| `location4`  | `string\|null` | None |  |
| `location5`  | `string\|null` | None |  |
| `location6`  | `string\|null` | None |  |
| `location7`  | `string\|null` | None |  |
| `max_days_old`  | `integer\|null` | None | The age of the oldest advertisment in days that will be returned. |
| `category`  | `string\|null` | None | The category tag, as returned by the "category" endpoint. |
| `sort_dir`  | `Literal['down','up']\|null` | None | The direction to order the search results. |
| `sort_by`  | `Literal['date','default','hybrid','relevance','salary']\|null` | None | The ordering of the search results. |
| `salary_min`  | `integer\|null` | None | The minimum salary we wish to get results for. |
| `salary_max`  | `integer\|null` | None | The maximum salary we wish to get results for. |
| `salary_include_unknown`  | `Literal['1']\|null` | None | If set it "1", jobs without a known salary are returned. |
| `full_time`  | `Literal['1']\|null` | None | If set to "1", only full time jobs will be returned. |
| `part_time`  | `Literal['1']\|null` | None | If set to "1", only part time jobs will be returned. |
| `contract`  | `Literal['1']\|null` | None | If set to "1", only contract jobs will be returned. |
| `permanent`  | `Literal['1']\|null` | None | If set to "1", only permanent jobs will be returned. |
| `company`  | `string\|null` | None | The canonical company name. This may be returned in a Adzuna::API::Response::Company object when a job is returned. A full list of allowed terms in not available through the API. |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |

## Other Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `workflow_id` :red_circle: | `string` | None | A stable identifier used for persisting in incremental mode |
| `logics` :red_circle: | `array\|null` | None | A list of functions called in sequence with each item pulled from the origin. Each function might either return it's argument or None to discard the item. Any item discarded is eventually not pushed to the target |
| `format`  | `Callable\|null` | None | A formatting function to apply on items pulled before the push |
| `callback`  | `Callable\|null` | None | Registers a callback function to be called at the of a successful execution |
| `persist`  | `boolean` | True | When False has the effect of running in dry mode. Items are pulled but not pushed to the target |
| `incremental`  | `boolean` | False | Controls the incremental reading execution mode |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors.v2 import Adzuna


logging.basicConfig(level=logging.INFO)


Adzuna.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        app_id=...,
        app_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        country=...,
        results_per_page=...,
        what=...,
        what_and=...,
        what_phrase=...,
        what_or=...,
        what_exclude=...,
        title_only=...,
        where=...,
        distance=...,
        location0=...,
        location1=...,
        location2=...,
        location3=...,
        location4=...,
        location5=...,
        location6=...,
        location7=...,
        max_days_old=...,
        category=...,
        sort_dir=...,
        sort_by=...,
        salary_min=...,
        salary_max=...,
        salary_include_unknown=...,
        full_time=...,
        part_time=...,
        contract=...,
        permanent=...,
        company=...,
    ),
    push_parameters=dict(
        board_key=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```