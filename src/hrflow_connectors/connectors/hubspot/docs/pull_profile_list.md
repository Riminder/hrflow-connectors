# Pull profile list
`Hubspot Contacts` :arrow_right: `HrFlow.ai Profiles`

Retrieves contacts from Hubspot via API and send them to a ***Hrflow.ai Source***.



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_hubspot_contact`](../connector.py#L37) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `access_token` :red_circle: | `str` | None | The token used to authenticate any API calls made for to your HubSpot account. |
| `limit`  | `int` | None | The maximum number of results to display per page. |
| `after`  | `str` | None | The paging cursor token of the last successfully read resource will be returned as the `paging.next.after` JSON property of a paged response containing more results. |
| `properties`  | `str` | firstname,lastname,date_of_birth,email,phone,company,address,zip,city,state,country | A comma separated list of the properties to be returned in the response. If any of the specified properties are not present on the requested object(s), they will be ignored. |
| `propertiesWithHistory`  | `str` | None | A comma separated list of the properties to be returned along with their history of previous values. If any of the specified properties are not present on the requested object(s), they will be ignored. Usage of this parameter will reduce the maximum number of objects that can be read by a single request. |
| `associations`  | `typing.List[str]` | None | A comma separated list of object types to retrieve associated IDs for. If any of the specified associations do not exist, they will be ignored. |
| `archived`  | `bool` | False | Whether to return only results that have been archived. |

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
from hrflow_connectors import Hubspot
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Hubspot.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        access_token="your_access_token",
        limit=0,
        after="your_after",
        properties="firstname,lastname,date_of_birth,email,phone,company,address,zip,city,state,country",
        propertiesWithHistory="your_propertiesWithHistory",
        associations=***,
        archived=False,
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