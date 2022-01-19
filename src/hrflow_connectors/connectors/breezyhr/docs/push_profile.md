# Push profile
`Hrflow.ai` :arrow_right: `Breezyhr`

`PushProfileAction` pushes a Hrflow.ai profile to your company's `Breezyhr` candidate pool via their ***Breezy API***.

🔗 [Documentation](https://developer.breezy.hr/docs/company-position-candidates-add)

| Endpoints | Description |
| --------- | ----------- |
| [Get company_id](https://developer.breezy.hr/docs/companies)          | Endpoint to retrieve the companies associated with the authenticated user in case the user didn't specify his company ID required to `PushProfileAction`, the request method is `GET`           |
| [Post candidate](https://developer.breezy.hr/docs/company-position-candidates-add)         |   Endpoint to Add a new candidate to a poisition, required parameters are company id and position id, the request method is `POST`    |
|[Update a candidate on a position](https://developer.breezy.hr/docs/company-position-candidate-update)           | Endpoint to update a candidate on a position,in case the candidate already exists, required parameters are company id, position id and candidate id, the request method is `PUT`|
|[Get candidate id](https://developer.breezy.hr/docs/company-candidates-search)| Endpoint to search that a candidate already exists to retrieve his id, required parameters are company id and candidate's email address, the request method is `GET`|

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

```python
from hrflow_connectors import Breezyhr

from hrflow_connectors import OAuth2EmailPasswordBody
from hrflow_connectors.utils.hrflow import Profile, Source

profile = Profile(key="PROFILE_KEY", source=Source(key="SOURCE_KEY"))

auth = OAuth2EmailPasswordBody(
            access_token_url="https://api.breezy.hr/v3/signin",
            email = settings["EMAIL"]
            password=settings["PASSWORD"],
        )

Breezyhr.push_profile(
    auth=auth,
    subdomain=settings["SUBDOMAIN"],
    hrflow_email="MY_EMAIL",
    hrflow_secret="MY_X_API_KEY",
    company_name=settings["MY_COMPANY_NAME"],
    position_id=settings["MY_POSITION_ID"]
    profile=profile,
    )
```