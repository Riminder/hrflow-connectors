# Push profile
`Hrflow.ai` :arrow_right: `Bullhorn`

`PushProfileAction` pushes a Hrflow.ai profile to `Bullhorn` via their API.

**Links to Flatchr documentation on the endpoints used :**

| Endpoints | Description |
| --------- | ----------- |
| [Push profile](http://bullhorn.github.io/rest-api-docs) | Push a candidate with minimum information, the request method is `POST` |
| [Enrich profile Skills](http://bullhorn.github.io/rest-api-docs) | Enrich profile Skills `POST` |
| [Enrich profile Expriences](http://bullhorn.github.io/rest-api-docs) | Enrich profile Expriences `POST` |
| [Enrich profile Educations](http://bullhorn.github.io/rest-api-docs) | Enrich profile Educations `POST` |

## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter . Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `profile` :red_circle: | `HrflowProfile` | Profile to push        |
| `auth` :red_circle: | `OAuth2Session` | Auth instance to identify and communicate with the platform        |
| `subdomain` :red_circle: | `str` | Subdomain Bullhorn just before bullhornstaffing.com. For example subdomain=my_subdomain.my in http://my_subdomain.my.bullhornstaffing.com/ABC        |

:red_circle: : *required* 

## Example
Let's take as an example in a [***CATCH workflow***](https://developers.hrflow.ai/docs/workflows#catch-setup).
```python
from hrflow_connectors import Bullhorn
from hrflow import Hrflow
from hrflow_connectors import OAuth2Session
from hrflow_connectors.utils.schemas import HrflowProfile

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
profile = HrflowProfile(key="PROFILE_KEY", source=dict(key="SOURCE_KEY"))
auth = OAuth2Session(auth_code_url="https://auth.bullhornstaffing.com/oauth/authorize",
                    access_token_url="https://auth.bullhornstaffing.com/oauth/token",
                    session_token_url="https://rest.bullhornstaffing.com/rest-services/login",
                    client_id="MY_CLIENT_ID",
                    client_secret="MY_CLIENT_SECRET",
                    username="MY_USERNAME",
                    password="MY_PASSWORD",
                    name="BhRestToken")

Bullhorn.push_profile(
    auth=auth,
    subdomain="rest91",
    hrflow_client=hrflow_client(),
    profile=profile,
)
```