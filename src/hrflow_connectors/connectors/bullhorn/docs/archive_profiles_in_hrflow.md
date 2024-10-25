# Archive profiles in hrflow
`Bullhorn Read Profiles` :arrow_right: `HrFlow.ai Write Profiles`

Retrieves profiles from Bullhorn and archive them in Hrflow.ai source



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_item_to_be_archived`](../connector.py#L264) | Formatting function |
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
| `limit`  | `int` | None | Number of items to pull, ignored if not provided. |
| `last_modified_date` :red_circle: | `<class 'datetime.datetime'>` | None | The modification date from which you want to pull profiles |
| `query`  | `str` | isDeleted:0 | This query will restrict the results retrieved from Bullhorn based on the specified conditions |
| `fields`  | `str` | id | Field to be used as reference for archiving |

## Push Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Bullhorn
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Bullhorn.archive_profiles_in_hrflow(
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
        limit=0,
        last_modified_date=***,
        query="isDeleted:0",
        fields="id",
        read_mode=ReadMode.sync,
    ),
    push_parameters=dict(
        source_key="your_source_key",
    ),
    format=lambda *args, **kwargs: None # Put your code logic here,
    logics=[],
)
```