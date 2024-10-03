# Push profile
`HrFlow.ai Profiles` :arrow_right: `Lever Profiles`

Writes a profile from the Hrflow.ai Source to Lever via the API.



**Lever Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Post Profile**](https://{client_domain}.lever.co/v1/opportunities) | Endpoint to create a new profile |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_opportunity`](../connector.py#L382) | Formatting function |
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
| `auth_domain` :red_circle: | `str` | None | Auth domain for authenticating with Lever API, exemple: sandbox-lever |
| `client_domain` :red_circle: | `str` | None | Client domain for authenticating with Lever API, exemple: api.sandbox |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Lever API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Lever API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `perform_as` :red_circle: | `str` | None | User ID on behalf of whom the create action should be performed |
| `parse`  | `bool` | False | If true, parse resume for autofilling |
| `perform_as_posting_owner`  | `bool` | False | If true, set Opportunity owner to posting owner |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Lever
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Lever.push_profile(
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
        auth_domain="your_auth_domain",
        client_domain="your_client_domain",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        perform_as="your_perform_as",
        parse=False,
        perform_as_posting_owner=False,
    )
)
```