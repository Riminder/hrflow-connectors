
# Catch candidates
`SmartRecruiters Jobs` :arrow_right: `HrFlow.ai Profile Parsing`

Retrieves a candidate via the ***Broadbean*** API and send them to ***Hrflow.ai Parsing API***.


**SmartRecruiters Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get candidate**](https://integrations.broadbean.com/hc/en-us/articles/115004599649-Receiving-a-Candidate) | This will be a route on the receiving application or integration and not the candidate hub. This route will accept standard candidate data and return a standard response to be consumed by the hub. If parsing is enabled, two additional candidate documents will be included. The raw parsed XML as a tagged_cv and a JSON document of type parsed_cv (both Base64 encoded). |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile_catch`](../connector.py#L78) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `secret_key` :red_circle: | `str` | None | secret provided by Broadbean |
| `api_key` :red_circle: | `str` | None | The API Key used to generate the signature. This is provided by Broadbean at the time of setup |
| `profile`  | `Any` | None | Optional profile for testing |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `only_insert`  | `bool` | False | When enabled the profile is written only if it doesn't exist in the source |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Broadbean
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Broadbean.catch_candidates(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        secret_key="your_secret_key",
        api_key="your_api_key",
        profile=***,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        only_insert=False,
    )
)
```