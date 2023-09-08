# Pull job list
`TalentSoft Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from TalentSoft vacancies export API and send them to a ***Hrflow.ai Board***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_ts_vacancy`](../connector.py#L29) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access TalentSoft API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access TalentSoft API |
| `client_url` :red_circle: | `str` | None | URL of TalentSoft client integration |
| `q`  | `str` | None | Query search to get vacancies |
| `filter`  | `str` | None | Filter to apply when reading vacancies. See documentation at https://developers.cegid.com/api-details#api=cegid-talentsoft-recruiting-matchingindexation&operation=api-exports-v1-vacancies-get . . You can filter by **chronoNumber**, **updateDate**, **reference** **vacancyStatus**, **clientVacancyStatus**, **publicationMedia**  **publishedOnTheMedia**. Examples : By reference Single Item 'reference::2019-01'; By reference Multiple Items 'reference::2019-01,2019-02,2019-03';  By updateDate updated before the 10th of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber greater than 108921  'chronoNumber:gt:108921' .  |
| `max_read`  | `<class 'pydantic.types.PositiveInt'>` | 100 | The maximum number of jobs to pull during the execution. Proper tuning of this parameter should allow to control the execution time and avoid overtimes |

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
from hrflow_connectors import TalentSoft
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


TalentSoft.pull_job_list(
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
        q="your_q",
        filter="your_filter",
        max_read=100,
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