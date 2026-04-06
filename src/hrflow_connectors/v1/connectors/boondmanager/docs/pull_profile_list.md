# Pull profile list
`BoondManager Candidates` :arrow_right: `HrFlow.ai Profiles`

Retrieves all candidates from BoondManager and sends them to an HrFlow.ai Source.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_candidate`](../connector.py#L198) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_key` :red_circle: | `str` | None | BoondManager client key used to sign the JWT. Found in Administration → API settings. |
| `client_token` :red_circle: | `str` | None | BoondManager client token. |
| `user_token` :red_circle: | `str` | None | BoondManager user token for the account making API calls. |
| `language`  | `str` | fr | Language code used when resolving IDs to human-readable labels via the application dictionary (e.g. 'fr', 'en'). |
| `candidate_states`  | `str` | None | Comma-separated list of candidate state IDs to include. Leave empty to retrieve all states. BoondManager state IDs: 0=À traiter, 1=En cours de process, 2=Vivier, 3=Converti en Ressource, 4=Si projet, 5=Ne plus contacter, 6=Proposition en cours, 7=À recontacter plus tard, 9=Top profil. Example: '0,1,2,3,4,6,9'. |
| `limit`  | `int` | None | Maximum number of candidates to pull. Leave empty to pull all. Useful for testing or incremental runs. |
| `fetch_resume`  | `bool` | False | When True, the connector will attempt to download the most recent resume file for each candidate and attach it as a base64-encoded resume URL. This significantly increases runtime due to one extra API call per candidate. |

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
from hrflow_connectors import BoondManager
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BoondManager.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        client_key="your_client_key",
        client_token="your_client_token",
        user_token="your_user_token",
        language="fr",
        candidate_states="your_candidate_states",
        limit=0,
        fetch_resume=False,
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