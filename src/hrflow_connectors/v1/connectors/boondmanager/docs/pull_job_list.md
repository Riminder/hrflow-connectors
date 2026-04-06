# Pull job list
`BoondManager Opportunities` :arrow_right: `HrFlow.ai Jobs`

Retrieves all opportunities (job openings) from BoondManager and sends them to an HrFlow.ai Board.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_opportunity`](../connector.py#L115) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_key` :red_circle: | `str` | None | BoondManager client key used to sign the JWT. Found in Administration → API settings. |
| `client_token` :red_circle: | `str` | None | BoondManager client token. |
| `user_token` :red_circle: | `str` | None | BoondManager user token for the account making API calls. |
| `language`  | `str` | fr | Language code used when resolving IDs to human-readable labels via the application dictionary (e.g. 'fr', 'en'). |
| `opportunity_states`  | `str` | None | Comma-separated list of opportunity state IDs to filter on. Leave empty to retrieve all states. Example: '0,1,2'. |
| `limit`  | `int` | None | Maximum number of opportunities to pull. Leave empty to pull all. Useful for testing or incremental runs. |

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
from hrflow_connectors import BoondManager
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BoondManager.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        client_key="your_client_key",
        client_token="your_client_token",
        user_token="your_user_token",
        language="fr",
        opportunity_states="your_opportunity_states",
        limit=0,
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