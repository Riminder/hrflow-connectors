# Update applications in hrflow
`HrFlow.ai Profiles` :arrow_right: `Bullhorn Applications`

Retrieves profiles from Hrflow.ai and writes their applications on the Bullhorn source



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_application`](../connector.py#L380) | Formatting function |
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
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `profile_key` :red_circle: | `str` | None | HrFlow.ai profile key |

## Push Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `job_id` :red_circle: | `str` | None | id for the job in Bullhorn |
| `status_when_created` :red_circle: | `str` | None | The status of the application when created in Bullhorn |
| `source`  | `str` | None | The source of the application to be created in Bullhorn |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Bullhorn
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Bullhorn.update_applications_in_hrflow(
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
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    push_parameters=dict(
        job_id="your_job_id",
        status_when_created="your_status_when_created",
        source="your_source",
    ),
    format=lambda *args, **kwargs: None # Put your code logic here,
    logics=[],
    read_mode=ReadMode.sync
)
```