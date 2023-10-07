# Pull profile list

`CornerstoneOnDemand Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieve Candidates from Cornerstone OnDemand and index them into a HrFlow source.

**CornerstoneOnDemand Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get Job Applicant**](https://{corpname}{env}.csod.com/services/api/Recruiting/JobApplicant) | This API allows you to retrieve job applicants in a given status. Note that this API does not include applicant custom fields in the response. To retrieve applicant custom fields, please use the Get Job Applicant with Custom Fields API. |

## Action Parameters

| Field       | Type                                                                        | Default                                                       | Description                                                                                                                                                                                                                                     |
| ----------- | --------------------------------------------------------------------------- | ------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logics`    | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | []                                                            | List of logic functions                                                                                                                                                                                                                         |
| `format`    | `typing.Callable[[typing.Dict], typing.Dict]`                               | [`format_cornerstone_ondemand_profile`](../connector.py#L109) | Formatting function                                                                                                                                                                                                                             |
| `read_mode` | `str`                                                                       | ReadMode.sync                                                 | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field                        | Type                                                                                                   | Default | Description                                                                                     |
| ---------------------------- | ------------------------------------------------------------------------------------------------------ | ------- | ----------------------------------------------------------------------------------------------- |
| `authData` :red_circle:      | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandAuthentication'>` | None    | All the data needed in order to obtain an access token for Cornerstone OnDemand Recruiting API. |
| `CurrentStatus` :red_circle: | `str`                                                                                                  | None    | Valid Status.                                                                                   |
| `StatusDate`                 | `<class 'datetime.datetime'>`                                                                          | None    | Local Datetime Value. Format should be yyyy-mm-dd.                                              |
| `CsodGUID`                   | `str`                                                                                                  | None    | Applicant's GUID Value.                                                                         |
| `FirstName`                  | `str`                                                                                                  | None    | Applicant's First Name.                                                                         |
| `LastName`                   | `str`                                                                                                  | None    | Applicant's Last Name.                                                                          |
| `RequisitionID`              | `str`                                                                                                  | None    | Job Requisition Id.                                                                             |
| `JobTitle`                   | `str`                                                                                                  | None    | Job Requision Title.                                                                            |

## Destination Parameters

| Field                           | Type               | Default | Description                                                                 |
| ------------------------------- | ------------------ | ------- | --------------------------------------------------------------------------- |
| `api_secret` :red_circle:       | `str`              | None    | X-API-KEY used to access HrFlow.ai API                                      |
| `api_user` :red_circle:         | `str`              | None    | X-USER-EMAIL used to access HrFlow.ai API                                   |
| `source_key` :red_circle:       | `str`              | None    | HrFlow.ai source key                                                        |
| `edit`                          | `bool`             | False   | When enabled the profile must exist in the source                           |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None    | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : _required_

## Example

```python
import logging
from hrflow_connectors import CornerstoneOnDemand
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


CornerstoneOnDemand.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        authData=***,
        CurrentStatus="Draft",
        StatusDate=***,
        CsodGUID="your_CsodGUID",
        FirstName="your_FirstName",
        LastName="your_LastName",
        RequisitionID="your_RequisitionID",
        JobTitle="your_JobTitle",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        edit=False,
        only_edit_fields=***,
    )
)
```
