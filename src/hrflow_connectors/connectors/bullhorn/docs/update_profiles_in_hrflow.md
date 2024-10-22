# Update profiles in hrflow
`Bullhorn Update Profils` :arrow_right: `HrFlow.ai Profiles`

Retrieves profiles from Bullhorn and update them in Hrflow.ai source



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`profile_format`](../connector.py#L275) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Connector Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client identifier for Bullhorn |
| `client_secret` :red_circle: | `str` | None | Client secret identifier for Bullhorn |
| `password` :red_circle: | `str` | None | Password for Bullhorn login |
| `username` :red_circle: | `str` | None | Username for Bullhorn login |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |

## Pull Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `limit`  | `int` | None | Number of items to pull, ignored if not provided. |
| `fields`  | `<class 'hrflow_connectors.core.warehouse_v2.ConstrainedStrValue'>` | address,businessSectors,categories,companyName,customInt4,customInt5,customInt6,customText1,customText10,customText11,customText12,customText13,customText14,customText15,customText16,customText18,customText23,customText24,customText25,customText4,customText5,customText6,customText9,dateAdded,dateAvailable,dateAvailableEnd,dateLastModified,dateOfBirth,dayRate,dayRateLow,degreeList,desiredLocations,description,disability,educations,email,email2,employmentPreference,ethnicity,experience,firstName,id,lastName,mobile,name,namePrefix,occupation,owner,phone,primarySkills,secondaryOwners,secondarySkills,salary,salaryLow,skillSet,source,specialties,status,userDateAdded,veteran,willRelocate,workHistories,workPhone | List of profile fields to be retrieved from Bullhorn |
| `last_modified_date` :red_circle: | `<class 'datetime.datetime'>` | None | The modification date from which you want to pull profiles |
| `parse_resume`  | `bool` | False | If True, resumes will be retrieved and parsed along with the profile data |

## Push Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `source_key` :red_circle: | `str` | None | HrFlow.ai source key |
| `only_edit_fields`  | `typing.List[str]` | None | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import Bullhorn
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Bullhorn.update_profiles_in_hrflow(
    workflow_id="some_string_identifier",
    connector_auth=dict(
        client_id="your_client_id",
        client_secret="your_client_secret",
        password="your_password",
        username="your_username",
    ),
    hrflow_auth=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
    ),
    pull_parameters=dict(
        limit=0,
        fields="address,businessSectors,categories,companyName,customInt4,customInt5,customInt6,customText1,customText10,customText11,customText12,customText13,customText14,customText15,customText16,customText18,customText23,customText24,customText25,customText4,customText5,customText6,customText9,dateAdded,dateAvailable,dateAvailableEnd,dateLastModified,dateOfBirth,dayRate,dayRateLow,degreeList,desiredLocations,description,disability,educations,email,email2,employmentPreference,ethnicity,experience,firstName,id,lastName,mobile,name,namePrefix,occupation,owner,phone,primarySkills,secondaryOwners,secondarySkills,salary,salaryLow,skillSet,source,specialties,status,userDateAdded,veteran,willRelocate,workHistories,workPhone",
        last_modified_date=***,
        parse_resume=False,
    ),
    push_parameters=dict(
        source_key="your_source_key",
        only_edit_fields=***,
    ),
    format=lambda *args, **kwargs: None # Put your code logic here,
    logics=[],
    read_mode=ReadMode.sync
)
```