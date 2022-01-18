# Push profile
`Hrflow.ai` :arrow_right: `Breezyhr`

`PushProfileAction` pushes a Hrflow.ai profile to your company's `Breezyhr` candidate pool via their ***Breezy API***.

🔗 [Documentation](https://developer.breezy.hr/docs/company-position-candidates-add)

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `OAuth2EmailPasswordBody` | Auth instance to identify and communicate with the platform        |
| `position_id` :red_circle: | `str` | Id of the position to create a new candidate for, required.      |
| `company_name` | `Optional[str]` | Name of the company associated with the authenticated user, required if you haven't specified your company id. Default value `None`       |
| `company_id` | `Optional[str]` | Id of the company associated with the authenticated user, Default value `None`      |
| `origin` | `Optional[str]` | Indicates if the candidate is `sourced` or `applied`, Default value `sourced`      |
| `cover_letter` | `Optional[str]` | Candidate cover letter, Default value `None`      |


:red_circle: : *required*

## Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow_connectors import Breezyhr

from hrflow import Hrflow
from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors.utils.hrflow import Profile
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

        auth = OAuth2EmailPasswordBody(
            access_token_url="https://api.breezy.hr/v3/signin",
            email = settings["EMAIL"]
            password=settings["PASSWORD"],
        )

        response = Breezyhr.push_profile(
            auth=auth,
            subdomain=settings["SUBDOMAIN"],
            hrflow_client=client,
            profile=profile,
        )
        return response
```