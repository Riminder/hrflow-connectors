# Pull job list
`Jobadder Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs from Jobadder and writes them to an Hrflow.ai board 



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L20) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `authorization_url` :red_circle: | `str` | None | Authorization URL for obtaining authorization code |
| `client_id` :red_circle: | `str` | None | Client ID for authenticating with Jobadder API |
| `client_secret` :red_circle: | `str` | None | Client secret for authenticating with Jobadder API |
| `authorization_code` :red_circle: | `str` | None | Authorization code for obtaining access token |
| `redirect_uri` :red_circle: | `str` | None | Redirect URI for obtaining access token |
| `jobId` :red_circle: | `typing.List[int]` | None | Array of integers for Job Id (optional) |
| `jobTitle`  | `str` | None | Job title (optional) |
| `companyId` :red_circle: | `typing.List[int]` | None | Alias for company.companyId (optional) |
| `contactId` :red_circle: | `typing.List[int]` | None | Contact Id (optional) |
| `statusId` :red_circle: | `typing.List[int]` | None | Job status ID (optional) |
| `active`  | `bool` | None | Search for active/open jobs (optional) |
| `userFavourite`  | `bool` | None | Search for the user's favorite jobs (optional) |
| `folderId` :red_circle: | `typing.List[int]` | None | Search in specific folders (optional) |
| `userId` :red_circle: | `typing.List[int]` | None | User ID - search for jobs by owner or associated recruiter (optional) |
| `ownerUserId` :red_circle: | `typing.List[int]` | None | User ID - search for jobs by owner (optional) |
| `recruiterUserId` :red_circle: | `typing.List[int]` | None | User ID - search jobs by associated recruiters (optional) |
| `createdBy` :red_circle: | `typing.List[int]` | None | User ID - search for jobs created by the specified user(s) (optional) |
| `createdAt` :red_circle: | `typing.List[str]` | None | Search for jobs created at a specific date and time (UTC assumed, ISO date-time) (optional) |
| `updatedBy` :red_circle: | `typing.List[int]` | None | User ID - search for jobs last updated by the specified user(s) (optional) |
| `updatedAt` :red_circle: | `typing.List[str]` | None | Search for jobs updated at a specific date and time (UTC assumed, ISO date-time) (optional) |
| `closedBy` :red_circle: | `typing.List[int]` | None | User ID - search for jobs last closed by the specified user(s) (optional) |
| `closedAt` :red_circle: | `typing.List[str]` | None | Search for jobs closed at a specific date and time (UTC assumed, ISO date-time) (optional) |
| `sort` :red_circle: | `typing.List[str]` | None | Sort the results by one or multiple fields. Prefix with '-' to sort descending (optional) |
| `fields` :red_circle: | `typing.List[str]` | None | Additional fields to include with the results (optional) |
| `embed` :red_circle: | `typing.List[str]` | None | Embed related resources (optional) |
| `limit`  | `int` | 100 | Number of items to fetch per request (max: 100) |
| `offset`  | `int` | 0 | Number of items to skip |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `sync`  | `bool` | True | When enabled only pushed jobs will remain in the board |
| `update_content`  | `bool` | False | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Jobadder
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Jobadder.pull_job_list(
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
        jobId=***,
        jobTitle="your_jobTitle",
        companyId=***,
        contactId=***,
        statusId=***,
        active=False,
        userFavourite=False,
        folderId=***,
        userId=***,
        ownerUserId=***,
        recruiterUserId=***,
        createdBy=***,
        createdAt=***,
        updatedBy=***,
        updatedAt=***,
        closedBy=***,
        closedAt=***,
        sort=***,
        fields=***,
        embed=***,
        limit=100,
        offset=0,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```