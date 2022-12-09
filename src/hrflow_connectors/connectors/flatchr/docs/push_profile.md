
# Push profile
`HrFlow.ai Profiles` :arrow_right: `Flatchr Profiles`

Writes profile from Hrflow.ai Source to Flatchr



**Flatchr Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Post Candidate**](https://developers.flatchr.io/docs/QuickStart/Candidats/Creer_un_candidat) | Endpoint to create a new candidate and assign to a talent pool, the request method is `POST` |


## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L150) | Formatting function |
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
| `auth` :red_circle: | `str` | None | Auth used to authenticate with an API key named `Authorization` in the headers (hash key), go to Paramètres avancée -> Avancé -> API |
| `vacancy` :red_circle: | `str` | None | The pool in which candidates will be placed. Findable in the URL of the job to which you want to add a candidate |
| `company` :red_circle: | `str` | None | The id of the company |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Flatchr
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Flatchr.push_profile(
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
        auth="your_auth",
        vacancy="your_vacancy",
        company="your_company",
    )
)
```