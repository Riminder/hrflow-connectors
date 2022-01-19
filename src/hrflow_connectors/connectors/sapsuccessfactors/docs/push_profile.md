# Push profile

`Hrflow.ai` :arrow_right: `SAP SuccessFactors`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***SAP SuccessFactors*** Jobs pool.

**Links to SAP documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
|[Post Candidate](https://api.sap.com/api/RCMCandidate/overview) | Endpoint to post a candidate with background information and profile, an api server is required, the request method is `POST`|
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

```python
from hrflow_connectors import SapSuccessfactors
from hrflow import Hrflow
from hrflow_connectors import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from hrflow_connectors.connectors.utils.hrflow import Profile, Source


client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))
auth = XAPIKeyAuth(
    name = "APIKey",
    value= settings['MY_API_KEY'],
)

SapSuccessfactors.push_profile(
    auth=auth,
    api_server="MY_API_SERVER",
    hrflow_client=client,
    profile=profile,
)
```