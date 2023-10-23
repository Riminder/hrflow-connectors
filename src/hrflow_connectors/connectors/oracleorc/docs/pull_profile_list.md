# Pull profile list

`OracleORC Profiles` :arrow_right: `HrFlow.ai Profiles`

Retrive Candidates from Oracle ORC into a HrFLow source

**OracleORC Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all recruiting candidates**](http://{host}:{port}/hcmRestApi/resources/11.13.18.05/recruitingCandidates) | |

## Action Parameters

| Field       | Type                                                                        | Default                                            | Description                                                                                                                                                                                                                                     |
| ----------- | --------------------------------------------------------------------------- | -------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logics`    | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | []                                                 | List of logic functions                                                                                                                                                                                                                         |
| `format`    | `typing.Callable[[typing.Dict], typing.Dict]`                               | [`format_oracleorc_profile`](../connector.py#L125) | Formatting function                                                                                                                                                                                                                             |
| `read_mode` | `str`                                                                       | ReadMode.sync                                      | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field                      | Type                                                                            | Default | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| -------------------------- | ------------------------------------------------------------------------------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `authData` :red_circle:    | `<class 'hrflow_connectors.connectors.oracleorc.schemas.OracleORCAuth'>`        | None    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `hostAndPort` :red_circle: | `<class 'hrflow_connectors.connectors.oracleorc.schemas.OracleORCHostAndPort'>` | None    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `expand`                   | `str`                                                                           | None    | When this parameter is provided, the specified children are included in the resource payload (instead of just a link). The value of this query parameter is "all" or "". More than one child can be specified using comma as a separator. Example: ?expand=Employees,Localizations. Nested children can also be provided following the format "Child.NestedChild" (Example: ?expand=Employees.Managers). If a nested child is provided (Example: Employees.Managers), the missing children will be processed implicitly. For example, "?expand=Employees.Managers" is the same as "?expand=Employees,Employees.Managers" (which will expand Employees and Managers).                                                                                                                                                                                                                              |
| `fields`                   | `str`                                                                           | None    | This parameter filters the resource fields. Only the specified fields are returned, which means that if no fields are specified, no fields are returned (useful to get only the links). If an indirect child resource is provided (Example: Employees.Managers), the missing children will be processed implicitly. For example, "?fields=Employees.Managers:Empname" is the same as "?fields=;Employees:;Employees.Managers:Empname" (which will only return the "Empname" field for Managers). the value of this query parameter is a list of resource fields. The attribute can be a direct (Example: Employees) or indirect (Example: Employees.Managers) child. It cannot be combined with expand query parameter. If both are provided, only fields will be considered. Format: ?fields=Attribute1,Attribute2. Format for fields in child resource: ?fields=Accessor1:Attribute1,Attribute2 |
| `finder`                   | `str`                                                                           | None    | Used as a predefined finder to search the collection.<br><br>Format ?finder=\<finderName>;\<variableName>=\<variableValue>,\<variableName2>=\<variableValue2><br><br>The following are the available finder names and corresponding finder variables<br><br> - PrimaryKey Finds all candidates using a unique ID.<br> Finder Variables<br> - CandidateNumber; string; Unique ID and candidate number used to find candidates.<br> - findByPersonId Finds all candidates using a person ID.<br> Finder Variables<br> - personId; string; Person ID used to find candidates.<br> - search Finds all candidates using search.<br> Finder Variables<br> - keywords; string; Search and keywords; string; used to find candidates.                                                                                                                                                                     |
| `limit`                    | `<class 'hrflow_connectors.core.warehouse.ConstrainedIntValue'>`                | None    | This parameter restricts the number of resources returned inside the resource collection. If the limit exceeds the resource count then the framework will only return the available resources.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `links`                    | `str`                                                                           | None    | This parameter can be used to show only certain links while accessing a singular resource or a resource collection. The parameter value format is a comma-separated list of : \<link_relation>. Example: self,canonical                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `offset`                   | `<class 'hrflow_connectors.core.warehouse.ConstrainedIntValue'>`                | 0       | Used to define the starting position of the resource collection. If offset exceeds the resource count then no resources are returned. Default value is 0.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `onlyData`                 | `bool`                                                                          | None    | The resource item payload will be filtered in order to contain only data (no links section, for example).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `q`                        | `str`                                                                           | None    | This query parameter defines the where clause. The resource collection will be queried using the provided expressions. The value of this query parameter is one or more expressions. Example: ?q=Deptno>=10 and <= 30;Loc!=NY<br><br>Format: ?q=expression1;expression2<br><br>You can use these queryable attributes to filter this collection resource using the q query parameter:<br><br> - CandLastModifiedDate; string; Candidate Last Modified Date<br> - CandidateType; string; Candidate type in recruiting candidates.<br> - FullName; string; Full name in recruiting candidates.<br> - LastName; string; Last name in recruiting candidates.<br> - PreferredTimezone; string; Preferred time zone of the candidate.                                                                                                                                                                   |
| `totalResults`             | `bool`                                                                          | False   | The resource collection representation will include the "estimated row count" when "?totalResults=true", otherwise the count is not included. The default value is "false".                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

## Destination Parameters

| Field                           | Type               | Default | Description                                                                 |
| ------------------------------- | ------------------ | ------- | --------------------------------------------------------------------------- |
| `api_secret` :red_circle:       | `str`              | None    | X-API-KEY used to access HrFlow.ai API                                      |
| `api_user` :red_circle:         | `str`              | None    | X-USER-EMAIL used to access HrFlow.ai API                                   |
| `source_key` :red_circle:       | `str`              | None    | HrFlow.ai source key                                                        |
| `edit`                          | `bool`             | False   | When enabled the profile must exist in the source                           |
| `only_edit_fields` :red_circle: | `typing.List[str]` | None    | List of attributes to use for the edit operation e.g. ['tags', 'metadatas'] |

:red*circle: : \_required*

## Example

```python
import logging
from hrflow_connectors import OracleORC
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


OracleORC.pull_profile_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        authData=***,
        hostAndPort=***,
        expand="your_expand",
        fields="your_fields",
        finder="your_finder",
        limit=***,
        links="your_links",
        offset=0,
        onlyData=False,
        q="your_q",
        totalResults=False,
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
