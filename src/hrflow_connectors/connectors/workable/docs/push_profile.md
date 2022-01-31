# Push profile

`Hrflow.ai` :arrow_right: `Workable`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Workable*** company endpoint and a optionally a Jobs pool.

**Links to Workable documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Create Candidate](https://workable.readme.io/docs/job-candidates-create) | Create a candidate with provided details,a job shortcode is required the request method is `POST`|

## Parameters

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
| `auth` :red_circle: | `Union[AuthorizationAuth, OAuth2PasswordCredentialsBody`] | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | cubdomain of a company endpoint in `https://{self.subdomain}.workable.com/spi/v3/jobs` for example subdomain=`eurostar` for eurostar company     |
| `shortcode` | `str` | Offer to which the candidate will be assigned.|

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Workable
from hrflow import Hrflow
from hrflow_connectors import AuthorizationAuth
from hrflow_connectors.utils.schemas import HrflowProfile

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
profile = Profile(key="PROFILE_KEY", source=dict(key="SOURCE_KEY"))
auth = AuthorizationAuth(
    value= "MY_BEARER_TOKEN",
)

Workable.push_profile(
    subdomain="MY_SUBDOMAIN",
    shortcode="MY_JOB_SHORTCODE",
    auth=auth,
    hrflow_client=client,
    profile=profile,
)
```