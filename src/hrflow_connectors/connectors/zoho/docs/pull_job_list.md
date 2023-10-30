# Pull job list
`Zoho Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from zoho  and writes them to an Hrflow.ai board



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job_openings`](../connector.py#L20) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `selectColumns`  | `str` | All | Columns to fetch |
| `fromIndex`  | `int` | 1 | Index of the first job to fetch |
| `toIndex`  | `int` | 200 | Index of the last job to fetch |
| `sortColumnString`  | `str` | Modified Time | Column to sort by |
| `sortOrderString`  | `str` | desc | Sort order |
| `lastModifiedTime`  | `str` | None | Fetch jobs modified after this time |
| `newFormat`  | `int` | 1 | whiche format to use for response |
| `version`  | `int` | 2 | API version |
| `accounts_url` :red_circle: | `str` | None | URL for fetching accounts |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Lever API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Lever API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `publishURL`  | `bool` | False | Whether to include publishURL in the response |

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
from hrflow_connectors import Zoho
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Zoho.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        selectColumns="All",
        fromIndex=1,
        toIndex=200,
        sortColumnString="Modified Time",
        sortOrderString="desc",
        lastModifiedTime="your_lastModifiedTime",
        newFormat=1,
        version=2,
        accounts_url="your_accounts_url",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        publishURL=False,
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