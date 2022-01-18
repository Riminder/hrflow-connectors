# Push job
`HrFlow.ai` :arrow_right: `Monster`

`PushJobAction` pushes a Monster job to `Hrflow.ai`.

🔗 [Documentation](https://partner.monster.com/real-time-posting-devguide)

## Parameters

| Field                        | Type | Description |
|------------------------------| ---- | ----------- |
| `auth` :red_circle:          | `MonsterBodyAuth` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle:     | String | Subdomain monster just before `monster.com`. For example subdomain=`my_subdomain.my` in `https://my_subdomain.my.monster.com:8443/bgwBroker`       |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `job` :red_circle:           | `Job` | Job to push        |


:red_circle: : *required* 

### Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow_connectors import Monster

from hrflow import Hrflow
from hrflow_connectors.utils.logger import get_logger_with_basic_config



def workflow(_request, settings):
    """
    CATCH Workflow
    """    
    # We add a basic configuration to our logger to see the messages displayed in the standard output
    # This is not mandatory. It allows you to see what the connector is doing.
    logger = get_logger_with_basic_config()

    event = EventParser(request=_request)
    job = event.get_job()
    if job is not None:
        logger.info("job found !")

        client = Hrflow(api_secret=settings["X-API-KEY"], api_user=settings["X-USER-EMAIL"])
        auth = MonsterBodyAuth(username=credentials["monster"]["username"],
        password=credentials["monster"]["password"])

        action = Monster.catch_profile(
            auth=auth,
            subdomain="gateway",
            hrflow_client=hrflow_client(),
            job=job,
        )
        response = action.execute()