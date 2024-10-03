# Pull profile list
`DigitalRecruiters Read Profils` :arrow_right: `HrFlow.ai Profile Parsing`

Retrieves all profiles from Digital Recruiters and sends them to an Hrflow.ai Source.


**DigitalRecruiters Read Profils endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Read Profiles**]({url_environnement}/public/v1/{endpoint}) | Read profiles from Digital Recruiters |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_dr_profile`](../connector.py#L217) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_key` :red_circle: | `str` | None | DigitalRecruiters API key |
| `username` :red_circle: | `str` | None | Username for authentication |
| `password` :red_circle: | `str` | None | Password for authentication |
| `environment_url` :red_circle: | `<class 'pydantic.networks.HttpUrl'>` | None | URL environment for the API |
| `jobAd`  | `int` | None | Optional: Id of a job advertisement |
| `sort`  | `str` | None | Optional: Field to sort by (id, firstName, lastName, createdAt, updatedAt) |
| `limit`  | `int` | 50 | Optional: Limit the number of results returned |
| `page`  | `int` | 1 | Optional: Page number of results returned |

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
from hrflow_connectors import DigitalRecruiters
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


DigitalRecruiters.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_key="your_api_key",
        username="your_username",
        password="your_password",
        environment_url=***,
        jobAd=0,
        sort="your_sort",
        limit=50,
        page=1,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        only_insert=False,
    )
)
```