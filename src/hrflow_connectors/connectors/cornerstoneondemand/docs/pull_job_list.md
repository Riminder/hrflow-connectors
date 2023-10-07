# Pull job list

`CornerstoneOnDemand Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieve Job Requisitions from Cornerstone OnDemand and index them into a HrFlow board.

**CornerstoneOnDemand Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get Job Requisition**](https://{corpname}{env}.csod.com/services/api/Recruiting/JobRequisitionDetails) | This API allows you to retrieve job requisition details from CornerstoneOnDemand Recruiting. The API supports retrieving a single job requisition as well as using query filters to retrieve multiple job requisitions in a single call. If you are using the lastModifiedSince query parameter, please note that this parameter does not account for changes made to job postings. It only accounts for changes made to the job requisition itself. Any changes made in the Create/Edit Job Requisition General page are accounted for by the lastModifiedSince filter. The FromDate and ToDate parameters allow you to search for job requisition by initial creation date. The API does not include requisition custom fields in the response. To retrieve custom fields, please use the `Get Job Requisition Custom Field` API. The API does not resolve any job ad tags to their actual values in the ExternalAd, InternalAd, and MobileAd response fields. In order to retrieve resolved job ads, please use the `Get Job Requisition Ad Details` API. To learn more about job ad tags in CornerstoneOnDemand Recruiting, please see this Online Help article. You must be logged in to your CornerstoneOnDemand portal to access this article. |

## Action Parameters

| Field       | Type                                                                        | Default                                                  | Description                                                                                                                                                                                                                                     |
| ----------- | --------------------------------------------------------------------------- | -------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logics`    | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | []                                                       | List of logic functions                                                                                                                                                                                                                         |
| `format`    | `typing.Callable[[typing.Dict], typing.Dict]`                               | [`format_cornerstone_ondemand_job`](../connector.py#L49) | Formatting function                                                                                                                                                                                                                             |
| `read_mode` | `str`                                                                       | ReadMode.sync                                            | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field                   | Type                                                                                                   | Default | Description                                                                                     |
| ----------------------- | ------------------------------------------------------------------------------------------------------ | ------- | ----------------------------------------------------------------------------------------------- |
| `authData` :red_circle: | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandAuthentication'>` | None    | All the data needed in order to obtain an access token for Cornerstone OnDemand Recruiting API. |
| `Title`                 | `str`                                                                                                  | None    | Job Title.                                                                                      |
| `ReqId`                 | `str`                                                                                                  | None    | Requisition Id.                                                                                 |
| `FromDate`              | `<class 'datetime.datetime'>`                                                                          | None    | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.                                       |
| `ToDate`                | `<class 'datetime.datetime'>`                                                                          | None    | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.                                       |
| `lastModifiedSince`     | `<class 'datetime.datetime'>`                                                                          | None    | UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.                                       |
| `DivisionId`            | `str`                                                                                                  | None    | Valid Division Id.                                                                              |
| `LocationId`            | `str`                                                                                                  | None    | Valid Location Id.                                                                              |
| `Statuses`              | `typing.List[hrflow_connectors.connectors.cornerstoneondemand.utils.enums.CornerstoneOnDemandStatus]`  | None    | Comma separated list of statuses. e.g. 'Draft,Open,Closed'.                                     |
| `Language`              | `str`                                                                                                  | None    | Language should include ISO language code. Example en-US, fr-FR, it-IT, en-GB...                |

## Destination Parameters

| Field                     | Type   | Default | Description                                                |
| ------------------------- | ------ | ------- | ---------------------------------------------------------- |
| `api_secret` :red_circle: | `str`  | None    | X-API-KEY used to access HrFlow.ai API                     |
| `api_user` :red_circle:   | `str`  | None    | X-USER-EMAIL used to access HrFlow.ai API                  |
| `board_key` :red_circle:  | `str`  | None    | HrFlow.ai board key                                        |
| `sync`                    | `bool` | True    | When enabled only pushed jobs will remain in the board     |
| `update_content`          | `bool` | False   | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`     | `bool` | False   | When enabled jobs are enriched with HrFlow.ai parsing      |

:red_circle: : _required_

## Example

```python
import logging
from hrflow_connectors import CornerstoneOnDemand
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


CornerstoneOnDemand.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        authData=***,
        Title="your_Title",
        ReqId="your_ReqId",
        FromDate=***,
        ToDate=***,
        lastModifiedSince=***,
        DivisionId="your_DivisionId",
        LocationId="your_LocationId",
        Statuses=***,
        Language="zh-HK",
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
