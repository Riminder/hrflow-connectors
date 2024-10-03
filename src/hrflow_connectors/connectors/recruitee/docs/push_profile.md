# Push profile
`HrFlow.ai Profiles` :arrow_right: `Recruitee Profiles`

Writes a profile from Hrflow.ai Source as a candidate on Recruitee via the API



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L39) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `profile_key` :red_circle: | `str` | None | HrFlow.ai profile key |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `company_id` :red_circle: | `str` | None | Company ID. A company subdomain can also be used. |
| `api_token` :red_circle: | `str` | None | Personal API Token allowing access to the Recruitee API from external services. |
| `recruitee_endpoint` :red_circle: | `str` | None | Specifies which endpoint to be used, satging or production. |
| `offer_ids`  | `typing.List[int]` | None | Offers to which the candidate will be assigned with default stage. You can also pass one ID as offer_id |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Recruitee
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Recruitee.push_profile(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        company_id="your_company_id",
        api_token="your_api_token",
        recruitee_endpoint="STAGING ENDPOINT",
        offer_ids=***,
    )
)
```