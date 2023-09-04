# Pull job list
`Recruitee Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***Recruitee*** API and send them to a ***Hrflow.ai Board***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L104) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `str` | None | Company ID. A company subdomain can also be used. |
| `api_token` :red_circle: | `str` | None | Personal API Token allowing access to the Recruitee API from external services. |
| `recruitee_endpoint` :red_circle: | `str` | None | Specifies which endpoint to be used, satging or production. |
| `kind`  | `str` | None | If no kind is given, returns all job offers, if kind is job then lists only jobs, if scope is talent_pool, lists only talent pools |
| `scope`  | `str` | None | If no scope is given list all job offers. archived returns only archived job offers, active returns published, internal and closed job offers, not_archived returns all but archived jobs |
| `view_mode`  | `str` | brief | default (default mode, includes most of offer details); brief (only offer’s id, title, status and kind) |

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
from hrflow_connectors import Recruitee
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Recruitee.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        company_id="your_company_id",
        api_token="your_api_token",
        recruitee_endpoint="STAGING ENDPOINT",
        kind="your_kind",
        scope="your_scope",
        view_mode="brief",
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