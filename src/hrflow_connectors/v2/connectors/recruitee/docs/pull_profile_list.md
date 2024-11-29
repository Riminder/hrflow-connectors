# Pull profile list
`Recruitee Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves all profiles via the ***Recruitee*** API and send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_to_hrflow_profile`](../connector.py#L55) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `str` | None | Company ID. A company subdomain can also be used. |
| `api_token` :red_circle: | `str` | None | Personal API Token allowing access to the Recruitee API from external services. |
| `recruitee_endpoint` :red_circle: | `str` | None | Specifies which endpoint to be used, satging or production. |
| `limit`  | `int` | None | Specifies the number of candidates to retrieve |
| `offset`  | `int` | None | Skip number of candidates from the begining, used for ‘load more’, offset for next page should be current offset + limit |
| `created_after`  | `str` | None | Show only candidates created after given date |
| `disqualified`  | `bool` | None | Show only disqualified candidates who are disqualified in at least one job (should be string ‘true’ or ‘1’). |
| `qualified`  | `bool` | None | Show only disqualified candidates who are qualified in at least one job (should be string ‘true’ or ‘1’). |
| `ids`  | `str` | None | List of IDs separated by comma, example: 234221,4211412,535432 |
| `offer_id`  | `str` | None | Filter by offer |
| `query`  | `str` | None | Search query for candidate’s name or offer |
| `sort`  | `str` | None | Sorting options: by_date, by_last_message |
| `with_messages`  | `bool` | None | Show only candidates with messages (should be string ‘true’ or ‘1’) |
| `with_my_messages`  | `bool` | None | Show only candidates with messages that current admin sent (should be string ‘true’ or ‘1’ |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `edit`  | `bool` | False | When enabled the profile must exist in the source |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Recruitee
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Recruitee.pull_profile_list(
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
        limit=0,
        offset=0,
        created_after="your_created_after",
        disqualified=False,
        qualified=False,
        ids="your_ids",
        offer_id="your_offer_id",
        query="your_query",
        sort="by_date",
        with_messages=False,
        with_my_messages=False,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        edit=False,
        only_edit_fields=***,
    )
)
```