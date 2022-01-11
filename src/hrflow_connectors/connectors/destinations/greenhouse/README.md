# Greenhouse Connector
**Greenhouse is an ATS.**

`Greenhouse` :arrow_right: `Hrflow.ai`

## PushProfile
`PushProfile` gets all available jobs listed on ***Greenhouse board***. It adds all these **jobs** to a ***Hrflow.ai Board***.

## SCHEMAS
add `schemas.py` as ***basemodel*** for a ***Greenhouse profile object***.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |
| `job_id` :red_circle: | `List[int]` | List of jobs internal ids to which the candidate should be added |
:red_circle: : *required* 
| `on_behalf_of` :red_circle: | `str` | The ID of the user sending the profile, or the person he is sending the profile on behalf of |

### Example

```python
from hrflow import Hrflow

from hrflow_connectors.connectors.destinations.greenhouse import PushProfile
from hrflow_connectors.connectors.core.auth import OAuth2PasswordCredentialsBody, AuthorizationAuth
from hrflow_connectors.connectors.utils.hrflow import EventParser, Profile, Source
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
        value= settings['Authorization']
    )

        action = PushProfile(
            auth=auth,
            job_id=settings["job_id"],
            on_behalf_of=settings["on_behalf_of"],
            hrflow_client=client,
            profile=profile,
        )
        response = action.execute()
        return response
```