# Monster Connector
**Monster is an ATS.**

`Monster` :arrow_right: `HrFlow`

Lists of actions :
* [**`PullProfile`**](#PullProfile)

## PullProfile

`PullProfile` catches a Monster profile to `Hrflow.ai`.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `source_key` :red_circle: | `String` | Source key where the profiles to be added will be stored        |
| `request` :red_circle: | `Dict[str, Any]` | Body to format in HrFlow Profile        |
| `file_field` | `String` | Name of the field containing the file       |
| `hrflow_client` :red_circle: | hrflow.Hrflow | Hrflow client instance used to communicate with the Hrflow.ai API        |


:red_circle: : *required* 

### Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow import Hrflow

from hrflow_connectors.connectors.destinations.flatchr import PullProfile
from hrflow_connectors.utils.hrflow import EventParser, Profile, Source
from hrflow_connectors.utils.logger import get_logger_with_basic_config



def workflow(_request, settings):
    """
    CATCH Workflow
    """    
    # We add a basic configuration to our logger to see the messages displayed in the standard output
    # This is not mandatory. It allows you to see what the connector is doing.
    logger = get_logger_with_basic_config()

    event = EventParser(request=_request)
    profile = event.get_profile()
    if profile is not None:
        logger.info("Profile found !")

        client = Hrflow(api_secret=settings["X-API-KEY"], api_user=settings["X-USER-EMAIL"])

        action = PullProfile(
            hrflow_client=hrflow_client(),
            request=profile,
            source_key="8df6a1247b1a95e0b84f5226093ff2c58e60cdf1",
            file_field="FileContents",
        )
        response = action.execute()
```