# Push profile

`HrFlow.ai Profiles` :arrow_right: `CornerstoneOnDemand Profiles`

Retrieve Profile from HrFlow and candidate for a Cornerstone OnDemand Job Requisition.

**CornerstoneOnDemand Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Create Candidate and Application**](https://{corpname}{env}.csod.com/services/api/x/candidate/v1/application) | In order to post an application, certain fields must be sent over as part of the application. The POST Candidate and Application endpoint is not dependent on the GET Application Workflow endpoint. This endpoint can be used to create a candidate and submit an application. Considerations: If an application is posted with a required Custom Integration, standard action item, integration, or additional attachment, the application will be posted in an incomplete status. Resumes added via the POST Candidate and Application endpoint are not parsed. If an additional attachment is posted using the POST Candidate and Application endpoint, the entire additional attachment section will be considered complete, even if more than one additional attachment is marked required in the application workflow. |

## Action Parameters

| Field       | Type                                                                        | Default                                         | Description                                                                                                                                                                                                                                     |
| ----------- | --------------------------------------------------------------------------- | ----------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logics`    | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | []                                              | List of logic functions                                                                                                                                                                                                                         |
| `format`    | `typing.Callable[[typing.Dict], typing.Dict]`                               | [`format_hrflow_profile`](../connector.py#L148) | Formatting function                                                                                                                                                                                                                             |
| `read_mode` | `str`                                                                       | ReadMode.sync                                   | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field                      | Type  | Default | Description                               |
| -------------------------- | ----- | ------- | ----------------------------------------- |
| `api_secret` :red_circle:  | `str` | None    | X-API-KEY used to access HrFlow.ai API    |
| `api_user` :red_circle:    | `str` | None    | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle:  | `str` | None    | HrFlow.ai source key                      |
| `profile_key` :red_circle: | `str` | None    | HrFlow.ai profile key                     |

## Destination Parameters

| Field                           | Type                                                                                                           | Default        | Description                                                                                                         |
| ------------------------------- | -------------------------------------------------------------------------------------------------------------- | -------------- | ------------------------------------------------------------------------------------------------------------------- |
| `authData` :red_circle:         | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandAuthentication'>`         | None           | All the data needed in order to obtain an access token for Cornerstone OnDemand Recruiting API.                     |
| `applicationPreferences`        | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandApplicationPreferences'>` | sendEmail=True |                                                                                                                     |
| `candidatePreferences`          | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandCandidatePreferences'>`   | None           |                                                                                                                     |
| `jobRequisitionId` :red_circle: | `str`                                                                                                          | None           | The ATS job requisition's identifier. This is a "ref" value and not the internal "id". A correct example is Req123. |
| `questions`                     | `typing.List[hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandQuestion]`            | None           | A collection of application submission data for questions of type: Disclaimer, Compliance and Prescreening.         |
| `source`                        | `<class 'hrflow_connectors.connectors.cornerstoneondemand.schemas.CornerstoneOnDemandSource'>`                 | None           |                                                                                                                     |

:red_circle: : _required_

## Example

```python
import logging
from hrflow_connectors import CornerstoneOnDemand
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


CornerstoneOnDemand.push_profile(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        authData=***,
        applicationPreferences=sendEmail=True,
        candidatePreferences=***,
        jobRequisitionId="your_jobRequisitionId",
        questions=***,
        source=***,
    )
)
```
