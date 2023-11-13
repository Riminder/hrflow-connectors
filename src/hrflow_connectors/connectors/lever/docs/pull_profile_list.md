# Pull profile list
`Lever Profiles` :arrow_right: `HrFlow.ai Profiles`

Read a profile from Lever Source to Hrflow.ai via the API.


**Lever Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all profiles**](https://{client_domain}.lever.co/v1/opportunities) | Endpoint to get the list of all profiles |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L281) | Formatting function |
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
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `edit`  | `bool` | False | When enabled the profile must exist in the source |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Lever
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Lever.pull_profile_list(
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
        source_key="your_source_key",
        edit=False,
        only_edit_fields=***,
    )
)
```