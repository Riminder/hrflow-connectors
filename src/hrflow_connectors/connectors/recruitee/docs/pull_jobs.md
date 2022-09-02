
# Pull jobs
`Recruitee Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***Recruitee*** API and send them to a ***Hrflow.ai Board***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L109) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `str` | None | Company ID. A company subdomain can also be used. |
| `API_Token` :red_circle: | `str` | None | Personal API Token allowing access to the Recruitee API from external services. |
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


logging.basicConfig(level=logging.INFO)


Recruitee.pull_jobs(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        company_id="your_company_id",
        API_Token="your_API_Token",
        kind="your_kind",
        scope="your_scope",
        view_mode=brief,
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