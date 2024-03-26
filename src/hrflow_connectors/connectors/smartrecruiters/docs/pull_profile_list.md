# Pull profile list
`SmartRecruiters Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves all profiles via the ***SmartRecruiter*** API and send them to a ***Hrflow.ai Source***.


**SmartRecruiters Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get candidate**](https://developers.smartrecruiters.com/reference/candidatesget-1) | Endpoint to get the content of a candidate with a given id, a candidateId parameter is required, the request method is `GET` |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_candidate`](../connector.py#L237) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `x_smart_token` :red_circle: | `str` | None | X-SmartToken used to access SmartRecruiters API |
| `query`  | `str` | None | keyword search, for more infromation see SmartRecruiters HelpCenter |
| `updated_after`  | `str` | None | ISO8601-formatted time boundaries for the candidate update time |
| `job_ids`  | `typing.List[str]` | None | List of job ids to filter candidates by |
| `tags`  | `typing.List[str]` | None | List of tags to filter candidates by |
| `onboarding_status`  | `str` | None | Onboarding status of a candidate |
| `status`  | `typing.List[str]` | None | candidate’s status filter in a context of a job |
| `limit`  | `int` | 100 | Number of items to pull from SmartRecruiters at a time. Not matter what value is supplied it is capped at 100 |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `edit`  | `bool` | False | When enabled the profile must exist in the source |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import SmartRecruiters
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


SmartRecruiters.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        x_smart_token="your_x_smart_token",
        query="your_query",
        updated_after="your_updated_after",
        job_ids=***,
        tags=***,
        onboarding_status="READY_TO_ONBOARD",
        status=***,
        limit=100,
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