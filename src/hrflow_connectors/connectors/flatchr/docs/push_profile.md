# Push profile
`Hrflow.ai` :arrow_right: `Flatchr`

`PushProfileAction` pushes a Hrflow.ai profile to `Flatchr` via their API.

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `AuthorizationAuth` | Auth instance to identify and communicate with the platform        |
| `vacancy` :red_circle: | `str` | The pool in which candidates will be placed. Findable in the URL        |
| `compagny` :red_circle: | `str` | The id of the compagny        |

:red_circle: : *required* 

## Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow_connectors import Flatchr

from hrflow import Hrflow
from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.utils.hrflow import EventParser
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
        auth = AuthorizationAuth(value=settings["FLATCHR_KEY"])
        response = Flatchr.push_profile(
            auth=auth,
            hrflow_client=client,
            profile=profile,
            vacancy="k0M5O9ylKZnxbQBy",
            company="LEZBvp5b4LdMoVmg",
        )
        return response
```