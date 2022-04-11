
# Pull jobs
`Crosstalent Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves all jobs via the ***Crosstalent*** API and send them to a ***Hrflow.ai Board***.


**Crosstalent Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all jobs**](https://vulcain-eng--preprod.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/) | Endpoint to search jobs by traditional params (offset, limit...) and get the list of all jobs with their ids, the request method is `GET` |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L150) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `env` :red_circle: | `str` | None | Environnement: test, stagin, production |
| `subdomain` :red_circle: | `str` | None | Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC` |
| `client_secret` :red_circle: | `str` | None | Crosstalent secret key |
| `client_id` :red_circle: | `str` | None | Crosstalent id |
| `username` :red_circle: | `str` | None | Username |
| `password` :red_circle: | `str` | None | Password |

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
from hrflow_connectors import Crosstalent


logging.basicConfig(level=logging.INFO)


Crosstalent.pull_jobs(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        env="your_env",
        subdomain="your_subdomain",
        client_secret="your_client_secret",
        client_id="your_client_id",
        username="your_username",
        password="your_password",
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