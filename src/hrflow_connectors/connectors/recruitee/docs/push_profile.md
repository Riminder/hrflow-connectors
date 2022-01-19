# Push profile

`Hrflow.ai` :arrow_right: `Recruitee`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Recruitee*** company endpoint and a optionally a Jobs pool.

**Links to Recruitee documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Create Candidate](https://docs.recruitee.com/reference/candidates-post) | Create a candidate with provided details,a company id is required however you can also use a company subdomain instead, the request method is `POST`|

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
| `company_id` :red_circle: | `str` | company_id of your company endpoint or the company you want to push profiles to in `https://api.recruitee.com/c/{company_id}/candidates`. A company subdomain can also be used, for example company_id=`testhr` for ***TESTHR*** an example company created to test     |
| `offer_id` | `Optional[List[int]]` | Offers to which the candidate will be assigned with default stage. You can also pass one ID as offer_id. Default value : `None`|

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Recruitee
from hrflow import Hrflow
from hrflow_connectors import AuthorizationAuth
from hrflow_connectors.utils.hrflow import Profile, Source

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))
auth = AuthorizationAuth(
    name = 'Authorization',
    value= settings['BEARER_TOKEN'],
)

Recruitee.push_profile(
    company_id = settings['MY_COMPANY_ID'],
    auth=auth,
    hrflow_client=client,
    profile=profile,
)
```