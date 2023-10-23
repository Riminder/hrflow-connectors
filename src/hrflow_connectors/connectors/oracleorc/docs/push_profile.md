# Push profile

`HrFlow.ai Profiles` :arrow_right: `OracleORC Profiles`

Retrive Profiles from HrFlow and create a candidate on Oracle ORC for a Job Requisition

**OracleORC Profiles endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Create a candidate**](http://{host}:{port}/hcmRestApi/resources/11.13.18.05/recruitingCandidates) | |

## Action Parameters

| Field       | Type                                                                        | Default                                        | Description                                                                                                                                                                                                                                     |
| ----------- | --------------------------------------------------------------------------- | ---------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `logics`    | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | []                                             | List of logic functions                                                                                                                                                                                                                         |
| `format`    | `typing.Callable[[typing.Dict], typing.Dict]`                               | [`format_hrflow_profile`](../connector.py#L47) | Formatting function                                                                                                                                                                                                                             |
| `read_mode` | `str`                                                                       | ReadMode.sync                                  | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field                      | Type  | Default | Description                               |
| -------------------------- | ----- | ------- | ----------------------------------------- |
| `api_secret` :red_circle:  | `str` | None    | X-API-KEY used to access HrFlow.ai API    |
| `api_user` :red_circle:    | `str` | None    | X-USER-EMAIL used to access HrFlow.ai API |
| `source_key` :red_circle:  | `str` | None    | HrFlow.ai source key                      |
| `profile_key` :red_circle: | `str` | None    | HrFlow.ai profile key                     |

## Destination Parameters

| Field                      | Type                                                                            | Default | Description |
| -------------------------- | ------------------------------------------------------------------------------- | ------- | ----------- |
| `authData` :red_circle:    | `<class 'hrflow_connectors.connectors.oracleorc.schemas.OracleORCAuth'>`        | None    |             |
| `hostAndPort` :red_circle: | `<class 'hrflow_connectors.connectors.oracleorc.schemas.OracleORCHostAndPort'>` | None    |             |

:red_circle: : _required_

## Example

```python
import logging
from hrflow_connectors import OracleORC
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


OracleORC.push_profile(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        authData=***,
        hostAndPort=***,
    )
)
```
