## Push profile

`Hrflow.ai` :arrow_right: `Greenhouse`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Greenhouse*** Jobs pool..

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `Union[AuthorizationAuth, OAuth2PasswordCredentialsBody`] | Auth instance to identify and communicate with the platform        |
| `job_id` :red_circle: | `List[int]` | List of jobs internal ids to which the candidate should be added |
| `on_behalf_of` :red_circle: | `str` | The ID of the user sending the profile, or the person he is sending the profile on behalf of |

:red_circle: : *required* 

### Example

```python
from hrflow_connectors import Greenhouse

from hrflow import Hrflow
from hrflow_connectors.connectors import OAuth2PasswordCredentialsBody, AuthorizationAuth
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


        auth = AuthorizationAuth(
        name = 'Authorization',
        value= settings['AUTHORIZATION'],
    )

        response = Greenhouse.push_profile(
            auth=auth,
            job_id=settings["JOB_ID"],
            on_behalf_of=settings["ON_BEHALF_OF"],
            hrflow_client=client,
            profile=profile,
        )
        return response
```