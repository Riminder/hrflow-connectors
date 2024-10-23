# Update jobs in hrflow
`Bullhorn Update Jobs` :arrow_right: `HrFlow.ai Jobs`

Pull jobs from Bullhorn and update them to Hrflow.ai Board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L201) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Connector Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client identifier for Bullhorn |
| `client_secret` :red_circle: | `str` | None | Client secret identifier for Bullhorn |
| `password` :red_circle: | `str` | None | Password for Bullhorn login |
| `username` :red_circle: | `str` | None | Username for Bullhorn login |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | User email used to access HrFlow.ai API |

## Pull Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `int` | None | Number of items to pull, ignored if not provided. |
| `fields`  | `<class 'hrflow_connectors.core.warehouse_v2.ConstrainedStrValue'>` | address,assignedUsers,businessSectors,categories,clientBillRate,clientContact,clientCorporation,costCenter,customInt1,customInt2,customText1,customText10,customText11,customText12,customText13,customText2,customText3,customText4,customText5,customText6,customText7,customText8,customText9,customTextBlock1,customTextBlock2,customTextBlock3,customTextBlock4,customTextBlock5,dateAdded,dateEnd,degreeList,description,durationWeeks,educationDegree,employmentType,feeArrangement,hoursOfOperation,hoursPerWeek,isOpen,isWorkFromHome,markUpPercentage,numOpenings,onSite,payRate,salary,salaryUnit,skills,skillList,source,specialties,startDate,status,title,type,willRelocate | List of job fields to be retrieved from Bullhorn |
| `last_modified_date` :red_circle: | `<class 'datetime.datetime'>` | None | The modification date from which you want to pull jobs |

## Push Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Bullhorn
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Bullhorn.update_jobs_in_hrflow(
    workflow_id="some_string_identifier",
    connector_auth=dict(
        client_id="your_client_id",
        client_secret="your_client_secret",
        password="your_password",
        username="your_username",
    ),
    hrflow_auth=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
    ),
    pull_parameters=dict(
        limit=0,
        fields="address,assignedUsers,businessSectors,categories,clientBillRate,clientContact,clientCorporation,costCenter,customInt1,customInt2,customText1,customText10,customText11,customText12,customText13,customText2,customText3,customText4,customText5,customText6,customText7,customText8,customText9,customTextBlock1,customTextBlock2,customTextBlock3,customTextBlock4,customTextBlock5,dateAdded,dateEnd,degreeList,description,durationWeeks,educationDegree,employmentType,feeArrangement,hoursOfOperation,hoursPerWeek,isOpen,isWorkFromHome,markUpPercentage,numOpenings,onSite,payRate,salary,salaryUnit,skills,skillList,source,specialties,startDate,status,title,type,willRelocate",
        last_modified_date=***,
        read_mode=ReadMode.sync,
    ),
    push_parameters=dict(
        board_key="your_board_key",
    ),
    format=lambda *args, **kwargs: None # Put your code logic here,
    logics=[],
)
```