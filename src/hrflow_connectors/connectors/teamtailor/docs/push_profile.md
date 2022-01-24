# Push profile

`Hrflow.ai` :arrow_right: `Teamtailor`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Teamtailor*** candidate pool.

**Links to Teamtailor documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Push profile](https://docs.teamtailor.com/#8c5ab123-1a53-4e3b-959f-e3d6d6224d71) | Endpoint to create candidate using Teamtailor API, the request method is `POST` |
| [Get Profile](https://docs.teamtailor.com/#1e591a5a-99cc-4d17-aaeb-7744dde0dbb2) | Endpoint to get a candidate ID if his profile already exists, the request method is `GET` |
| [Update Profile](https://docs.teamtailor.com/#28b9f6cb-72f0-45fa-8f3c-a2004b54e609) | Endpoint to update a candidate profile if it already exists, the request method is `PATCH` |

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
| `sourced` | `Optional[bool]` | True if added by a recruiter without applying. Default value `False` |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Teamtailor
from hrflow import Hrflow
from hrflow_connectors import AuthorizationAuth
from hrflow_connectors.utils.hrflow import Profile, Source


client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))
auth = AuthorizationAuth(
    name = 'Authorization',
    value= 'MY_AUTHORIZATION',
)

response = Teamtailor.push_profile(
    auth=auth,
    hrflow_client=client,
    profile=profile,
)
return response
```