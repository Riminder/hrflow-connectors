# Pull profile list
`Greenhouse Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves all profiles via the ***Greenhouse*** API and send them to a ***Hrflow.ai Board***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_to_hrflow_profile`](../connector.py#L167) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth` :red_circle: | `str` | None | XAPIKeyAuth |
| `created_after`  | `str` | None | Return only candidates that were created at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `updated_after`  | `str` | None | Return only candidates that were updated at or after this timestamp. Timestamp must be in in ISO-8601 format. |
| `job_id`  | `str` | None | If supplied, only return candidates that have applied to this job. Will return both when a candidate has applied to a job and when they’re a prospect for a job. |
| `email`  | `str` | None | If supplied, only return candidates who have a matching e-mail address. If supplied with job_id, only return a candidate with a matching e-mail with an application on the job. If email and candidate_ids are included, candidate_ids will be ignored. |
| `candidate_ids`  | `str` | None | If supplied, only return candidates with matching ids. If supplied with job_id, only return a candidate with a matching id with an application on the job. If email and candidate_ids are included, candidate_ids will be ignored. |

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
from hrflow_connectors import Greenhouse
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Greenhouse.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        auth="your_auth",
        created_after="your_created_after",
        updated_after="your_updated_after",
        job_id="your_job_id",
        email="your_email",
        candidate_ids="your_candidate_ids",
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