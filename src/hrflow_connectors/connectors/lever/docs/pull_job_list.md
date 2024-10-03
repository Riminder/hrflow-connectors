# Pull job list
`Lever Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the Lever API and sends them to the Hrflow.ai Board.


**Lever Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all jobs**](https://{client_domain}.lever.co/v1/postings) | Endpoint to get the list of all jobs |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L114) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `auth_domain` :red_circle: | `str` | None | Auth domain for authenticating with Lever API, exemple: sandbox-lever |
| `client_domain` :red_circle: | `str` | None | Client domain for authenticating with Lever API, exemple: api.sandbox |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Lever API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Lever API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `limit`  | `int` | 100 | Number of jobs to fetch per request (max: 100) |

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
from hrflow_connectors import Lever
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Lever.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        auth_domain="your_auth_domain",
        client_domain="your_client_domain",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        limit=100,
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