# Push job
`HrFlow.ai` :arrow_right: `Monster`

`PushJobAction` pushes a Monster job to `Hrflow.ai`.

🔗 [Documentation](https://partner.monster.com/real-time-posting-devguide)
| Endpoints | Description |
| --------- | ----------- |
|[`Push Jobs`](https://partner.monster.com/real-time-posting-devguide)|Endpoint to real time posting of jobs in Monster  |

## Parameters

| Field                        | Type | Description |
|------------------------------| ---- | ----------- |
| `auth` :red_circle:          | `MonsterBodyAuth` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle:     | String | Subdomain monster just before `monster.com`. For example subdomain=`my_subdomain.my` in `https://my_subdomain.my.monster.com:8443/bgwBroker`       |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `job` :red_circle:           | `Job` | Job to push        |


:red_circle: : *required* 


```python
from hrflow_connectors import Monster

from hrflow import Hrflow

event = EventParser(request=_request)
job = event.get_job()

auth = MonsterBodyAuth(username=credentials["monster"]["username"],
password=credentials["monster"]["password"])

Monster.catch_profile(
    auth=auth,
    subdomain="gateway",
    hrflow_email="MY_EMAIL",
    hrflow_secret="MY_X_API_KEY",
    job=job,
    )
