# Pull profile list
`Jobadder Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrieves profiles from Jobadder  and writes them to an Hrflow.ai source



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_profile`](../connector.py#L50) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `authorization_url` :red_circle: | `str` | None | Authorization URL for obtaining authorization code |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Jobadder API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Jobadder API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `redirect_uri` :red_circle: | `str` | None | Redirect URI for obtaining access token |
| `candidateId` :red_circle: | `typing.List[int]` | None | Candidate Id |
| `name`  | `str` | None | Candidate name |
| `email`  | `str` | None | Candidate email |
| `phone`  | `str` | None | Candidate phone or mobile number |
| `currentPosition`  | `str` | None | Current Position |
| `city`  | `str` | None | City |
| `state`  | `str` | None | State |
| `location`  | `str` | None | Location (city and/or state) |
| `dateOfBirth`  | `str` | None | Candidate date of birth available (ISO full-date) |
| `keywords`  | `str` | None | Search for key-words within the latest candidate resume |
| `partnerAction`  | `<class 'dict'>` | None | Partner Action parameters |
| `statusId` :red_circle: | `typing.List[int]` | None | Candidate status |
| `recruiterUserId` :red_circle: | `typing.List[int]` | None | User Id - search candidates by associated recruiters |
| `folderId` :red_circle: | `typing.List[int]` | None | Search in specific folders |
| `createdAt` :red_circle: | `typing.List[str]` | None | Search for candidates created at a specific date and time (UTC assumed, ISO date-time) |
| `updatedAt` :red_circle: | `typing.List[str]` | None | Search for candidates updated at a specific date and time (UTC assumed, ISO date-time) |
| `sort` :red_circle: | `typing.List[str]` | None | Sort the results by one or multiple fields, prefix with '-' to sort descending |
| `fields` :red_circle: | `typing.List[str]` | None | Additional fields to include with the results |
| `embed` :red_circle: | `typing.List[str]` | None | Embed related resources |
| `limit`  | `int` | 100 | Number of items to fetch per request (max: 100) |
| `offset`  | `int` | 0 | Number of items to skip |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `edit`  | `bool` | False | When enabled the profile must exist in the source |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Jobadder
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Jobadder.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        authorization_url="your_authorization_url",
        client_id="your_client_id",
        client_secret="your_client_secret",
        authorization_code="your_authorization_code",
        redirect_uri="your_redirect_uri",
        candidateId=***,
        name="your_name",
        email="your_email",
        phone="your_phone",
        currentPosition="your_currentPosition",
        city="your_city",
        state="your_state",
        location="your_location",
        dateOfBirth="your_dateOfBirth",
        keywords="your_keywords",
        partnerAction=***,
        statusId=***,
        recruiterUserId=***,
        folderId=***,
        createdAt=***,
        updatedAt=***,
        sort=***,
        fields=***,
        embed=***,
        limit=100,
        offset=0,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        edit=False,
        only_edit_fields=***,
    )
)
```