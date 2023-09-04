# Pull job list
`SAP Job` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***SAPSuccessFactors*** API and sends them to a ***Hrflow.ai Board***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L83) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_server` :red_circle: | `str` | None | Server to be accessed |
| `api_key` :red_circle: | `str` | None | API Key used to authenticate on the SAP API |
| `top`  | `int` | 100 | Show only the first N items value is capped at 100 |
| `skip`  | `int` | None | Search items by search phrases |
| `filter`  | `str` | None | Filter items by property values |
| `search`  | `str` | None | Search items by search phrases |

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
from hrflow_connectors import SAPSuccessFactors
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


SAPSuccessFactors.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_server="your_api_server",
        api_key="your_api_key",
        top=100,
        skip=0,
        filter="your_filter",
        search="your_search",
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