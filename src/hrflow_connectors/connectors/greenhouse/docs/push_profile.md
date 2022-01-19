# Push profile

`Hrflow.ai` :arrow_right: `Greenhouse`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Greenhouse*** Jobs pool.

| Endpoints | Description |
| --------- | ----------- |
| [Push profile](https://developers.greenhouse.io/harvest.html#post-add-candidate) | Endpoint to add candidate using greenhouse harvest API, the request method is `POST` |

## Parameters

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

## Example

```python
from hrflow_connectors import Greenhouse

from hrflow_connectors.connectors import OAuth2PasswordCredentialsBody, AuthorizationAuth
from hrflow_connectors.utils.hrflow import Profile, Source

profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))

auth = AuthorizationAuth(
name = 'Authorization',
value= settings['AUTHORIZATION'],
)

response = Greenhouse.push_profile(
    auth=auth,
    job_id=settings["JOB_ID"],
    on_behalf_of=settings["ON_BEHALF_OF"],
    hrflow_email="MY_EMAIL",
    hrflow_secret="MY_X_API_KEY",
    profile=profile,
)
return response
```