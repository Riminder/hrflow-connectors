# Catch profile
`Monster` :arrow_right: `HrFlow.ai`

`CatchProfileAction` catches a Monster profile to `Hrflow.ai`.


| Endpoints | Description |
| --------- | ----------- |
|[`Catch Profile`](https://partner.monster.com/apply-with-monster-implementing)|Endpoint to catch a profile application |
## Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `source_key` :red_circle: | `String` | Source key where the profiles to be added will be stored        |
| `request` :red_circle: | `Dict[str, Any]` | Body to format in HrFlow Profile        |


:red_circle: : *required* 

```python
from hrflow_connectors import Monster

from hrflow import Hrflow

Monster.catch_profile(
    hrflow_email="MY_EMAIL",
    hrflow_secret="MY_X_API_KEY",
    request=_request,
    source_key="8df6a1247b1a95e0b84f5226093ff2c58e60cdf1",
    )
```