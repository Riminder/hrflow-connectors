# Applicant new
`TalentSoft Profiles` :arrow_right: `HrFlow.ai Profile Parsing`

Handle TalentSoft 'applicant_new' event by fetching profile from TalentSoft and sending it to HrFlow.ai Parsing API.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_ts_candidate`](../connector.py#L136) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access TalentSoft API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access TalentSoft API |
| `client_url` :red_circle: | `str` | None | URL of TalentSoft client integration |
| `filter`  | `str` | None | Filter to apply when reading profiles. See documentation at https://developers.cegid.com/api-details#api=cegid-talentsoft-recruiting-matchingindexation&operation=api-exports-v1-candidates-get . Examples : By id Single Item 'id::_TS-00001'; By id Multiple Items 'id::_TS-00001,_TS-00002'; By email 'email::john.doe@company.corp'; By updateDate updated before the 10th of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber greater than 108921  'chronoNumber:gt:108921' |
| `fileId`  | `str` | None | If provided only the attachment matching with fileId is left in 'attachments'. If not found all attachments are left. |

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
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


TalentSoft.applicant_new(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        client_id="your_client_id",
        client_secret="your_client_secret",
        client_url="your_client_url",
        filter="your_filter",
        fileId="your_fileId",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
    )
)
```