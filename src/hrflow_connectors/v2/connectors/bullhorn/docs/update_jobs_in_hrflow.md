# Update jobs in hrflow
`Bullhorn` :arrow_right: `HrFlow`

Send **updated** 'job(s)' _from_ Bullhorn _to_ HrFlow



## Bullhorn Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | Client identifier for Bullhorn |
| `client_secret` :red_circle: | `string` | None | Client secret identifier for Bullhorn |
| `password` :red_circle: | `string` | None | Password for Bullhorn login |
| `username` :red_circle: | `string` | None | Username for Bullhorn login |

## HrFlow Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (Bullhorn)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `integer\|null` | None | Number of items to pull, ignored if not provided. |
| `fields`  | `string` | address,assignedUsers,businessSectors,categories,clientBillRate,clientContact,clientCorporation,costCenter,customInt1,customInt2,customText1,customText10,customText11,customText12,customText13,customText2,customText3,customText4,customText5,customText6,customText7,customText8,customText9,customTextBlock1,customTextBlock2,customTextBlock3,customTextBlock4,customTextBlock5,dateAdded,dateEnd,degreeList,description,durationWeeks,educationDegree,employmentType,feeArrangement,hoursOfOperation,hoursPerWeek,isOpen,isWorkFromHome,markUpPercentage,numOpenings,onSite,payRate,salary,salaryUnit,skills,skillList,source,specialties,startDate,status,title,type,willRelocate,owner | List of job fields to be retrieved from Bullhorn |
| `query`  | `string` | isDeleted:0 AND isOpen:true | This query will restrict the results retrieved from Bullhorn based on the specified conditions |
| `last_modified_date` :red_circle: | `string` | None | The modification date from which you want to pull jobs |

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
from hrflow_connectors.v2 import Bullhorn


logging.basicConfig(level=logging.INFO)


Bullhorn.update_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
        password=...,
        username=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        limit=...,
        fields=...,
        query=...,
        last_modified_date=...,
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