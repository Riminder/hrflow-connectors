# Create jobs in hrflow
`Bullhorn Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Bullhorn and writes them to Hrflow.ai Board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L190) | Formatting function |
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
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |

## Pull Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `last_modified_date` :red_circle: | `<class 'datetime.datetime'>` | None | The modification date from which you want to pull jobs |
| `fields` :red_circle: | `typing.List[str]` | None | List of job fields to be retrieved from Bullhorn |
| `query` :red_circle: | `<class 'hrflow_connectors.connectors.bullhorn.warehouse.Query'>` | None | A query composed of conditions combined using AND or OR |

## Push Parameters

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
        last_modified_date=***,
        fields=***,
        query=***,
    ),
    push_parameters=dict(
        board_key="your_board_key",
        enrich_with_parsing=False,
    ),
    format=lambda *args, **kwargs: None # Put your code logic here,
    logics=[],
    read_mode=ReadMode.sync
)
```