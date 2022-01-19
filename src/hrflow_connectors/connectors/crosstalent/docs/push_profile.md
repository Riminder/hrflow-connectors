# Push profile
`Hrflow.ai` :arrow_right: `Crosstalent`

`PushProfileAction` pushes a Hrflow.ai profile to `Crosstalent` via their ***Salesforce API***.

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `OAuth2PasswordCredentialsBody` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`        |

:red_circle: : *required*

## Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow_connectors import Crosstalent

from hrflow import Hrflow
from hrflow_connectors import OAuth2PasswordCredentialsBody
from hrflow_connectors.utils.hrflow import Profile, Source


client = Hrflow(api_secret=settings["X-API-KEY"], api_user=settings["X-USER-EMAIL"])

auth = OAuth2PasswordCredentialsBody(
    access_token_url="https://test.salesforce.com/services/oauth2/token",
    client_id=settings["CLIENT_ID"],
    client_secret=settings["CLIENT_SECRET"],
    username=settings["USERNAME"],
    password=settings["PASSWORD"],
)

Crosstalent.push_profile(
    auth=auth,
    subdomain=settings["SUBDOMAIN"],
    hrflow_client=client,
    profile=profile,
    )
```