# Push job
`HrFlow.ai` :arrow_right: `Monster`

`PushJobAction` pushes a Monster job to `Hrflow.ai`.

**Links to Monster documentation on the endpoints used :**

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
from hrflow.utils.hrflow import Job, Board

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

job = Job(key="MY_JOB_KEY", board=Board("MY_BOARD_KEY"))

auth = MonsterBodyAuth(username="MY_USER_NAME",password="MY_PASSWORD")

Monster.catch_profile(
    auth=auth,
    subdomain="gateway",
    hrflow_client=client,
    job=job,
)
