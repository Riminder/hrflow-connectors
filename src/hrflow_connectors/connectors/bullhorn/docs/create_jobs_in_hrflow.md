# Create jobs in hrflow
`Bullhorn Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Bullhorn and writes them to Hrflow.ai Board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L190) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `last_modified_date` :red_circle: | `str` | None | Last Modified Date in timestamp |
| `fields` :red_circle: | `str` | None | Fields to be retrieved from Bullhorn |
| `query` :red_circle: | `str` | None | the query parameters |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Bullhorn
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Bullhorn.create_jobs_in_hrflow(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        last_modified_date="your_last_modified_date",
        fields="your_fields",
        query="your_query",
    ),
    target_parameters=dict(
        board_key="your_board_key",
        enrich_with_parsing=False,
    )
)
```