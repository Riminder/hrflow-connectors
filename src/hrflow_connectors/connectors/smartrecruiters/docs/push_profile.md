# Push profile
`Hrflow.ai` :arrow_right: `SmartRecruiters`

`PushProfileAction` pushes a HrFlow.ai profile from a ***Hrflow.ai Source*** to `SmartRecruiters` via the ***SmartRecruiter*** API.


| Endpoints | Description |
| --------- | ----------- |
| [Post Candidate](https://dev.smartrecruiters.com/customer-api/live-docs/candidate-api/) | Endpoint to create a new candidate and assign to a talent pool, the request method is `POST` |

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `Profile` | Profile to push        |
| `auth` :red_circle: | `XSmartTokenAuth` | Auth instance to identify and communicate with the platform       |
| `job_id` :red_circle: | `str` | Id of a Job to which you want to assign a candidate when it’s created. For example job_id=`78d3ef91-8868-4ff3-b35d-9debf9d6f31f` in `https://api.smartrecruiters.com/jobs/78d3ef91-8868-4ff3-b35d-9debf9d6f31f/candidates`        |

:red_circle: : *required* 

## Example

```python
from hrflow_connectors import SmartRecruiters
from hrflow import Hrflow
from hrflow_connectors import XSmartTokenAuth
from hrflow_connectors.utils.hrflow import Profile, Source
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

auth = XSmartTokenAuth(value=settings["MY_SMART_TOKEN"])
profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))

SmartRecruiters.push_profile(
    auth=auth,
    job_id="3696cad0-a9b0-4a40-9cd7-4cc5feb1a509",
    hrflow_client=client,
    profile=profile,
)
```