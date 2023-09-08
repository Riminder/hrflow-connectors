# Push profile
`HrFlow.ai Profiles` :arrow_right: `Hubspot Contacts`

Writes a profile from Hrflow.ai Source as a contact on Hubspot via the API



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L16) | Formatting function |
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
| `access_token` :red_circle: | `str` | None | The token used to authenticate any API calls made for to your HubSpot account. |
| `dealID`  | `int` | None |  |
| `ticketID`  | `int` | None |  |
| `pipeline`  | `<class 'hrflow_connectors.connectors.hubspot.warehouse.Pipeline'>` | None |  |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Hubspot
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Hubspot.push_profile(
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
        access_token="your_access_token",
        dealID=0,
        ticketID=0,
        pipeline=***,
    )
)
```