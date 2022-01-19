# Push profile

`Hrflow.ai` :arrow_right: `Taleez`

`PushProfileAction` pushes a `Profile` from a ***HrFlow Source*** to a ***Taleez*** Jobs pool.

**Links to Taleez documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Add Candidate](https://api.taleez.com/swagger-ui/index.html?configUrl=/openapi.json/swagger-config#/candidates/create_1) | Endpoint for sourcing use case, to create a new candidate from his personal information, the request method is `Post` |
## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `XTaleezAuth` | Auth instance to identify and communicate with the platform        |
| `job_id` :red_circle: | `Optional[int]` | ID of the job to add a candidate to. Default value : `None` |
| `recruiter_id` :red_circle: | `int` | ID of the person recruiting the candidate, mandatory|


:red_circle: : *required* 

## Example

```python
from hrflow_connectors import Taleez
from hrflow import Hrflow
from hrflow_connectors import XTaleezAuth
from hrflow_connectors.utils.hrflow import Profile, Source
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))

auth = XTaleezAuth(value='MY_X_TALEEZ_API_KEY')

Taleez.push_profile(
    auth=auth,
    add_candidate_to_job=False,
    recruiter_id="MY_RECRUITER_ID",
    hrflow_client=client,
    profile=profile,
    )
```