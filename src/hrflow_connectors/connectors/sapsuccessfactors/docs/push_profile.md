# Push profile

`Hrflow.ai` :arrow_right: `SAP SuccessFactors`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***SAP SuccessFactors*** Jobs pool.

🔗 [Documentation](https://api.sap.com/api/RCMCandidate/overview)

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `Union[XAPIKeyAuth, OAuth2PasswordCredentialsBody`] | Auth instance to identify and communicate with the platform        |
| `api_server` :red_circle: | `str` | api_server: the `api_server` in `https://{api-server}/odata/v2`. For example api_server=`apisalesdemo8.successfactors.com` in `https://apisalesdemo8.successfactors.com/odata/v2`        |

:red_circle: : *required* 

## Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).

```python
from hrflow_connectors import SapSuccessfactors

from hrflow import Hrflow
from hrflow_connectors.connectors.core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from hrflow_connectors.connectors.utils.hrflow import Profile
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


        auth = XAPIKeyAuth(
            name = "APIKey",
        value= settings['MY_API_KEY'],
    )

        response = SapSuccessfactors.push_profile(
            auth=auth,
            api_server="MY_API_SERVER",
            hrflow_client=client,
            profile=profile,
        )
        return response
```