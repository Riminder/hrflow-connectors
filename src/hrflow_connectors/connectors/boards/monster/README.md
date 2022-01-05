# Monster Connector
**Monster is an ATS.**

`Hrflow.ai` :arrow_right: `Monster`

Lists of actions : 
* [**`PushJob`**](#PushJob)
* [**`PullProfile`**](#PullProfile)

## PushJob

`PushJob` pushes a Hrflow.ai job to `Monster` via their API.

This connector sends job in XML format.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `Username` :red_circle: | `String` | Username of the monster account        |
| `Password` :red_circle: | String | Password of the monster account        |
| `subdomain` :red_circle: | String | Subdomain monster just before `monster.com`. For example subdomain=`my_subdomain.my` in `https://my_subdomain.my.monster.com:8443/bgwBroker`       |
| `hrflow_client` :red_circle: | hrflow.Hrflow | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `job` :red_circle: | `Job` | Job to push        |


:red_circle: : *required* 

### Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow import Hrflow

from hrflow_connectors.connectors.boards.monster.actions import PushJob
from hrflow_connectors.utils.hrflow import Job, Board



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

        action = PushJob(
            username=credentials["monster"]["job"]["username"],
            password=credentials["monster"]["job"]["password"],
            subdomain="gateway",
            hrflow_client=hrflow_client(),
            job=job,
        )
        response = action.execute()