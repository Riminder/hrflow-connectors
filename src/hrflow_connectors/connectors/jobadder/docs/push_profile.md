# Push profile
`HrFlow.ai Profiles` :arrow_right: `Jobadder Profiles`

Retrieves profiles from an Hrflow.ai source and writes them to Jobadder



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile_to_jobadder`](../connector.py#L133) | Formatting function |
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
| `authorization_url` :red_circle: | `str` | None | Authorization URL for obtaining authorization code |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Jobadder API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Jobadder API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `redirect_uri` :red_circle: | `str` | None | Redirect URI for obtaining access token |
| `XAllowDuplicates`  | `bool` | False | Allow duplicates |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Jobadder
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Jobadder.push_profile(
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
        authorization_url="your_authorization_url",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        redirect_uri="your_redirect_uri",
        XAllowDuplicates=False,
    )
)
```