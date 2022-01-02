# Flatchr Connector
**Flatchr is an ATS.**

`Hrflow.ai` :arrow_right: `Flatchr`

Lists of actions :
* [**`PushProfile`**](#pushprofile)
* [**`EnrichProfile`**](#enrichprofile)

## PushProfile

`PushProfile` pushes a Hrflow.ai profile to `Flatchr` via their API.

This connector sends **only basic information**:
* First name
* Last name
* Email
* CV

**To add more information to the profile**, you need to enrich the created profile with the [**`EnrichProfile`**](#enrichprofile) connector.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `AuthorizationAuth` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | Subdomain flatchr just before `flatchr.io`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.flatchr.io/ABC`        |
| `vacancy` :red_circle: | `str` | The pool in which candidates will be placed. Findable in the URL        |

:red_circle: : *required* 

### Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow import Hrflow

from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.connectors.destinations.flatchr import PushProfile
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

        auth = AuthorizationAuth(value=settings["FLATCHR_KEY"])

        action = PushProfile(
            auth=auth,
            subdomain="careers",
            hrflow_client=client,
            profile=profile,
            vacancy="k0M5O9ylKZnxbQBy",
        )
        response = action.execute()
```

## EnrichProfile

`EnrichProfile` allows to enrich a flatchr profile with information from a ***Hrflow.ai Profile***. 

⚠️ **The *Flatchr Profile* must first be created with for example the [**`PushProfile`**](#pushprofile) connector.**

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `AuthorizationAuth` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | Subdomain flatchr just before `flatchr.io`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.flatchr.io/ABC`        |
| `vacancy` :red_circle: | `str` | The pool in which candidates will be placed. Findable in the URL        |
| `compagny` :red_circle: | `str` | The id of the compagny        |

:red_circle: : *required* 

### Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow import Hrflow

from hrflow_connectors.core.auth import AuthorizationAuth
from hrflow_connectors.connectors.destinations.flatchr import PushProfile, EnrichProfile
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

        auth = AuthorizationAuth(value=settings["FLATCHR_KEY"])

        push_profile_action = PushProfile(
            auth=auth,
            subdomain="careers",
            hrflow_client=client,
            profile=profile,
            vacancy="k0M5O9ylKZnxbQBy",
        )
        push_profile_action.execute()

        enrich_profile_action = EnrichProfile(
            auth=auth,
            subdomain="api",
            hrflow_client=client,
            profile=profile,
            vacancy="k0M5O9ylKZnxbQBy",
            compagny="LEZBvp5b4LdMoVmg",
        )
        response = enrich_profile_action.execute()
        return response
```