
# Applicant new
`TalentSoft Profiles` :arrow_right: `HrFlow.ai Profile Parsing`

Handle TalentSoft 'applicant_new' event by fetching profile from TalentSoft and sending it to HrFlow.ai Parsing API.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_ts_candidate`](../connector.py#L24) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access TalentSoft API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access TalentSoft API |
| `client_url` :red_circle: | `str` | None | URL of TalentSoft client integration |
| `applicantId` :red_circle: | `str` | None | TalentSoft applicantId of the profile to fetch |
| `fileId`  | `str` | None | If provided only the attachment matching with fileId is left in 'attachments'. If not found all attachments are left |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import TalentSoft


logging.basicConfig(level=logging.INFO)


TalentSoft.applicant_new(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        client_id="your_client_id",
        client_secret="your_client_secret",
        client_url="your_client_url",
        applicantId="your_applicantId",
        fileId="your_fileId",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
    )
)
```