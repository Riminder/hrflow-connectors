# 📖 Documentation
## Introduction to HrFlow.ai Connectors' framework

The framework is based on three main concepts : `Warehouse`, `ConnectorAction` and `Connector` :
- `Warehouse` is conceptually any source of data that you can either `read` from and/or `write` to
- `ConnectorAction` is an abstraction on top of two warehouses. An `origin` warehouse where data will be read from and a `target` warehouse where data will be written to
- `Connector` is an abstraction to group many `ConnectorAction`s

When contributing a new `Connector` you mainly have to define the corresponding `Warehouse`. You are bringing the knowledge of how to _read_ and/or _write_ to that new source of data. The rest is mainly boilerplate code and wire connecting with another already defined `Warehouse`.

## Prerequisites
Before you can develop new connectors or update existing ones you need to have `python` _3.10.5_ and [poetry](https://python-poetry.org/docs/) installed.

Then to install dependencies run `poetry install` and that's it.

## Connector Developpment Tutorial `LocalJSON`
_For the purpose of this tutorial we will be defining a `LocalJSON` connector with a _read-write_ interface. It should read data from a local JSON file and write data locally to a JSON file._

### Folder structure
We start by creating a new module `localjson` under [`src/hrflow_connectors/connectors`](src/hrflow_connectors/connectors/) with the three main files `warehouse.py`, `schemas.py` and `connector.py` :
```
.
└── hrflow-connectors/
    ├── src/
    │   └── hrflow_connectors/
    │       ├── connectors/
                ├── localjson/
                    ├── __init__.py
                    ├── warehouse.py
                    ├── connector.py
                    └── schemas.py
```

### `warehouse.py`
The `LocalJsonWarehouse` will have both `read` and `write` capability. The `read` action will be given a path to a JSON file to read from while the `write` action will be given a path to write the data received as a JSON file.

We start by importing core components from `hrflow_connectors`
```python
from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction, WarehouseWriteAction
```

To define a new `WarehouseReadAction` you need :
- A [pydantic](https://pydantic-docs.helpmanual.io/) schema that defines the arguments needed to read data from your warehoue. This is the place to define any necessary _tokens_ or _credentials_ for authentication but also any additional options or flags that can precise or scope the read operation. In our use case it's only the path to the JSON file to read from

```python
from pydantic import BaseModel, Field, FilePath


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")

```

- A callable that should accept four arguments:
    - A `LoggerAdapter` instance that should be used for logging 
    - An instance of the `pydantic` schema defined earlier
    - An optional `read_mode` enumeration member
    - An optional `read_from` string

The callable should return an _iterable_ that yields data as Python dictionnaries.

📢 _Both `read_mode` and `read_from` are not used in this basic example. **Yet they need to be present in the signature**. To learn more about how to use them to enable **incremental reading** see the [dedicated section](#how-to-do-incremental-reading)_

```python
import json
import typing as t
from json import JSONDecodeError
from logging import LoggerAdapter

from pydantic import BaseModel, Field, FilePath

from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction, WarehouseWriteAction


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")


def read(
    adapter: LoggerAdapter,
    parameters: ReadJsonParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    # Because of validation happening in ReadJsonParameters
    # no need to handle FileNotFoundError
    try:
        with open(parameters.path, "r") as f:
            data = json.load(f)
    except JSONDecodeError as e:
        message = "Invalid JSON file. Failed to decode with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)

    if isinstance(data, list):
        for item in data:
            yield item
    else:
        yield data

```
Defining the `WarehouseWriteAction` is in many aspects quite similar. Only the corresponding callable has a different signature.

:warning: _Mind that the underlying function is expected to **always** return the list of items for which the `write` operation failed_. If no such failure happened it should return an **_empty list_**.:warning:
```python
import json
import typing as t
from logging import LoggerAdapter
from pathlib import Path

from pydantic import BaseModel, Field

from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction, WarehouseWriteAction


class WriteJsonParameters(BaseModel):
    path: Path = Field(..., description="Path where to save JSON file")
    mode: t.Optional[t.Literal["append", "erase"]] = "erase"


def write(
    adapter: LoggerAdapter, parameters: WriteJsonParameters, items: t.Iterable[t.Dict]
) -> t.List[t.Dict]:
    failed_items = []
    items = list(items)
    try:
        if parameters.mode == "erase":
            with open(parameters.path, "w") as f:
                json.dump(items, f)
        else:
            try:
                with open(parameters.path, "r") as f:
                    old_items = json.load(f)
            except FileNotFoundError:
                old_items = []
            old_items.extend(items)
            with open(parameters.path, "w") as f:
                json.dump(old_items, f)
    # More error handling can be added to cope with file permissions for example
    except TypeError as e:
        message = "Failed to JSON encode provided items with error {}".format(repr(e))
        adapter.error(message)
        failed_items = items

    return failed_items
```

The last step is defining the `Warehouse`.
```python
LocalJSONWarehouse = Warehouse(
    name="LocalJSONWarehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadJsonParameters,
        function=read,
    ),
    write=WarehouseWriteAction(
        parameters=WriteJsonParameters,
        function=write,
    ),
)
```

> _`Warehouse` has also a `data_schema` attribute that can receive a [pydantic](https://pydantic-docs.helpmanual.io/) schema. This is for use cases where your warehouse stores data with a specific schema. We didn't use it here because any valid JSON can be read or written using `LocalJSONWarehouse`._

> ❗️ `data_type` expects one of the `DataType` enumeration members which are `job`, `profile` or `other`. Since _jobs_ and _profiles_ are quite common in the HR space they have dedicated members. This makes it easy to reason about warehouses on the HrFlow.ai plateform. For all other cases use `DataType.other`.

</br>
<details>
<summary>Whole content of <code>warehouse.py</code></summary>

```python
import json
import typing as t
from json import JSONDecodeError
from logging import LoggerAdapter
from pathlib import Path

from pydantic import BaseModel, Field, FilePath

from hrflow_connectors.core import (
    DataType,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")


def read(
    adapter: LoggerAdapter,
    parameters: ReadJsonParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    # Because of validation happening in ReadJsonParameters
    # no need to handle FileNotFoundError
    try:
        with open(parameters.path, "r") as f:
            data = json.load(f)
    except JSONDecodeError as e:
        message = "Invalid JSON file. Failed to decode with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)

    if isinstance(data, list):
        for item in data:
            yield item
    else:
        yield data


class WriteJsonParameters(BaseModel):
    path: Path = Field(..., description="Path where to save JSON file")
    mode: t.Optional[t.Literal["append", "erase"]] = "erase"


def write(
    adapter: LoggerAdapter, parameters: WriteJsonParameters, items: t.Iterable[t.Dict]
) -> t.List[t.Dict]:
    failed_items = []
    items = list(items)
    try:
        if parameters.mode == "erase":
            with open(parameters.path, "w") as f:
                json.dump(items, f)
        else:
            try:
                with open(parameters.path, "r") as f:
                    old_items = json.load(f)
            except FileNotFoundError:
                old_items = []
            old_items.extend(items)
            with open(parameters.path, "w") as f:
                json.dump(old_items, f)
    # More error handling can be added to cope with file permissions for example
    except TypeError as e:
        message = "Failed to JSON encode provided items with error {}".format(repr(e))
        adapter.error(message)
        failed_items = items

    return failed_items


LocalJSONWarehouse = Warehouse(
    name="LocalJSONWarehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadJsonParameters,
        function=read,
    ),
    write=WarehouseWriteAction(
        parameters=WriteJsonParameters,
        function=write,
    ),
)
```

</details>
</br>

### `schemas.py`
As stated above any valid JSON can be used with `LocalJSONWarehouse` so this file is left empty.

### `connector.py`

This last step is mainly boilerplate code and wire connecting. We will create two `ConnectorAction`s :
- `pull_jobs` : Uses the `LocalJSONWarehouse` to read jobs from a JSON file and sends them to a HrFlow.ai Job Board
- `push_profile` : Reads a profile from a HrFlow.ai Profile Source and writes it down to a local JSON file for inspection

**One important aspect** is that the whole logic of how to save jobs into a HrFlow.ai Job Board or how to pull a profile from a HrFlow.ai Profile source is abstracted by  `HrFlowJobWarehouse` and `HrFlowProfileWarehouse`. It's ready to use with a palette of optional parameters like adding parsing when pushing jobs for example.

It's as simple as
```python
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.localjson.warehouse import LocalJSONWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)

LocalJSON = Connector(
    name="LocalJSON",
    description="Read from JSON, Write to JSON",
    url="https://localjson.ai",
    actions=[
        ConnectorAction(
            name="pull_jobs",
            trigger_type=WorkflowType.pull,
            description="Send jobs from local JSON file to a ***Hrflow.ai Board***.",
            parameters=BaseActionParameters,
            origin=LocalJSONWarehouse,
            target=HrFlowJobWarehouse,
        ),
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description="Push a profile from a Hrflow.ai Source to a local JSON file",
            parameters=BaseActionParameters,
            origin=HrFlowProfileWarehouse,
            target=LocalJSONWarehouse,
        ),
    ],
)
```
:warning: :warning: **Mind that the `name` attribute of `Connector` once lowercased should match with its directory name i.e. `localjson` . If it's different using utility scritps to generate documentation or update manifest will fail.** :warning: :warning:

#### `ConnectorAction` in details
The two main components of `ConnectorAction` are the `origin` and `target` warehouses. Internally when a `ConnectorAction` is ran it will call the `read` function of the `origin` warehouse to pull items. Then the `write` function of the `target` warehouse is called to send the pulled items.

This flow can be controlled further by using two concepts `format` and `logics`.

##### `format`
`format` allows the user of your `ConnectorAction` to add a transformation step between _reading_ from the `origin` and _writing_ to the `target`. When you define your `ConnectorAction` with `parameters=BaseActionParameters` you are not supplying any default format function. This means that read items are written as is.

In the example of `pull_jobs` defined for `LocalJSON` connector it could be interesting to add a tag to the `job` in order to mark its origin. This can be acheived using a `format` function.

_Below pay attention to the `parameters` attribute of the `pull_jobs` `ConnectorAction`_.
```python
import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.localjson.warehouse import LocalJSONWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def format_job(job: t.Dict) -> t.Dict:
    origin_tag = dict(name="origin", value="localjson")
    job["tags"].append(origin_tag)
    return job


LocalJSON = Connector(
    name="JSON file",
    description="Read from JSON, Write to JSON",
    url="https://localjson.ai",
    actions=[
        ConnectorAction(
            name="pull_jobs",
            trigger_type=WorkflowType.pull,
            description="Send jobs from local JSON file to a ***Hrflow.ai Board***.",
            parameters=BaseActionParameters.with_defaults(
                "ReadJSONJobsActionParameters", format=format_job
            ),
            origin=LocalJSONWarehouse,
            target=HrFlowJobWarehouse,
        ),
    ],
)

```
By doing so when `pull_jobs` will be used by default the `format_job` function will be called and each job written to the HrFlow.ai Job Board should have the _origin tag_.

##### `logics`
The `logics` abstraction allows the user of your `ConnectorAction` to supply one or many decision functions that might for example filter out an item from being written to the `target` warehouse.
Each `logic` function should accept a single Python dictionnary argument representing an item that was _read_ from the `origin` warehouse. It should return either `None` to discard the item or a Python dictionnary which will be passed to the next `logic` function or _written_ to the `target` warehouse.

Below is an illustration with two `logic` functions.

```python
import typing as t


def only_males(item: t.Dict) -> t.Optional[t.Dict]:
    if item["gender"] == "male":
        return item


def only_over_18(item: t.Dict) -> t.Optional[t.Dict]:
    if item["age"] >= 18:
        return item


logics = [only_males, only_over_18]
# if read_items = [
#     dict(name="John", gender="male", age=12),
#     dict(name="Elena", gender="female", age=22),
#     dict(name="Mark", gender=None, age=32),
#     dict(name="Kevin", gender="male", age=29),
# ]
# Only Kevin will be eventually written to the target warehouse

```

_Mind_ that `logics` can not be defined at the `ConnectorAction` level contrary to `format` for which you can define a default behavior. `logics` are directly supplied by the end user when calling your `ConnectorAction`. See [section](#how-to-use-a-connector) for instructions on how to use a `Connector` and make us of the `logics` functions.

##### `trigger_type`
You can define two types of `ConnectorAction`s depending on how they will be eventually used on the HrFlow.ai [Workflows](https://developers.hrflow.ai/docs/workflows) plateform. For cases where the action should be executed on a schedule based interval choose `WorkflowType.pull`. Alternatively for actions that should be triggered by a HTTP request use `WorkflowType.catch`.

### Plugging `LocalJSON`
The last step for your connector to be fully integrated is to add it to the list of connectors in [`src/hrflow_connectors/__init__.py`](src/hrflow_connectors/__init__.py). Import your connector and add it to `__CONNECTORS__`.

### Add `LocalJson` to the HrFlow.ai Connectors' manifest
HrFlow.ai Connectors maintain a [`manifest.json`](./manifest.json) file at the root of the project. The manifest lists all the available connectors with their actions and useful metadata about how to use them.
In order to reflect the addition of `LocalJSON` to the list of connectors you need to run `make manifest` to update the [`manifest.json`](./manifest.json) file. You can check that `LocalJSON` is now listed.

### Generate documentation for `LocalJson`
Run `make docs` and you should see documentation generated in [`src/hrflow_connectors/connectors/localjson`](src/hrflow_connectors/connectors/localjson)

## How to use a Connector
**TL;DR** : Check the corresponding documentation in the connectors directory under [`src/hrflow_connectors/connectors/`](src/hrflow_connectors/connectors/)


A `Connector` is simply an abstraction with metadata on top of a list of `ConnectorAction`s. For each `ConnectorAction` that you add to your `Connector` you can call the action by running `{YourConnector}.{ConnectorAction.name}`.

For `LocalJSON` defined in the [tutorial above](#connector-developpment-tutorial-localjson) both `LocalJSON.pull_jobs(...)` and `LocalJSON.push_profile(...)` are valid instructions.

When called each `ConnectorAction` should be supplied with four **mandatory** arguments :
- `workflow_id`: any string identifier. It should be **unique** within your integration scope **if your are using _incremental_ reading mode**. See this [section](#how-to-do-incremental-reading) for more about this
- `action_parameters` : this is where you can supply `format`, `logics` and `read_mode`
- `origin_parameters` : this is where you supply any parameters needed to _read_ from the `origin` warehouse
- `target_parameters` : this is where you supply any parameters needed to _write_ to the `target` warehouse

To illustrate let's suppose that we want to :
- Use the `pull_jobs` action of `LocalJSON` connector
- The JSON file with jobs is located at `~/data/jobs_from_contractor_xxxyyy.json`
- We want each job sent to the HrFlow.ai Job Board to have a tag with the _origin_ and the name of the contractor _xxxyyy_
- We only want to send jobs that fall into the _Services_ and _Retail_ categories
- We only want to send jobs that were created during the last 30 days
- We want to use the AI Parsing Layer of HrFlow.ai and push to the Job Board `xxx444zzzjobkey`

This particular use case can be achieved by running the following code
```python
import logging
import typing as t
from datetime import datetime, timedelta

from hrflow_connectors import LocalJSON

logging.basicConfig(level=logging.INFO)


def format_job(job: t.Dict) -> t.Dict:
    origin_tag = dict(name="origin", value="localjson")
    contractor_tag = dict(name="contractor", value="xxxyyy")
    job["tags"].extend([origin_tag, contractor_tag])
    return job


def only_categories(job: t.Dict) -> t.Optional[t.Dict]:
    category_tag = next(
        (tag for tag in job["tags"] if tag["name"] == "Category"), dict(value=None)
    )
    if category_tag.get("value") in ["Services", "Retail"]:
        return job


def only_recent(job: t.Dict) -> t.Optional[t.Dict]:
    last_month = datetime.now() - timedelta(days=30)
    try:
        created_at = datetime.strptime(job["created_at"], "%Y-%m-%dT%H:%M:%S")
        if created_at >= last_month:
            return job
    except ValueError:
        return None


run_result = LocalJSON.pull_jobs(
    workflow_id="testing-localjson",
    action_parameters=dict(format=format_job, logics=[only_categories, only_recent]),
    origin_parameters=dict(path="~/data/jobs_from_contractor_xxxyyy.json"),
    target_parameters=dict(
        api_secret="your_hrflow.ai_api_secret",
        api_user="your_hrflow.ai_api_user",
        board_key="xxx444zzzjobkey",
        enrich_with_parsing=True,
    ),
)
```

<details>
<summary>Sample JSON file you can use for testing</summary>


```json
[
    {
        "name": "Accounting assistant",
        "reference": "testing_localjson_connector_1",
        "summary": "Accounting assistant Job",
        "created_at": "2022-01-22T11:54:49",
        "location": {
            "text": "Paris France"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Services"
            }
        ]
    },
    {
        "name": "Software engineer",
        "reference": "testing_localjson_connector_2",
        "summary": "Software engineer Job",
        "created_at": "2022-03-22T11:54:49",
        "location": {
            "text": "Tokyo Japan"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "IT"
            }
        ]
    },
    {
        "name": "Truck driver",
        "reference": "testing_localjson_connector_3",
        "summary": "Truck driver Job",
        "created_at": "2022-03-01T11:54:49",
        "location": {
            "text": "Sao Paulo Brazil"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Services"
            }
        ]
    },
    {
        "name": "Salesperson in sweets shop",
        "reference": "testing_localjson_connector_4",
        "summary": "Salesperson in sweets shop Job",
        "created_at": "2022-02-25T11:54:49",
        "location": {
            "text": "Beijing China"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Retail"
            }
        ]
    },
    {
        "name": "Teacher assistant",
        "reference": "testing_localjson_connector_5",
        "summary": "Teacher assistant Job",
        "created_at": "2021-03-22T11:54:49",
        "location": {
            "text": "Turin Italy"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Services"
            }
        ]
    },
    {
        "name": "Ranch farmer",
        "reference": "testing_localjson_connector_6",
        "summary": "Ranch farmer Job",
        "created_at": "2022-03-10T11:54:49",
        "location": {
            "text": "Seatle USA"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Argiculture"
            }
        ]
    },
    {
        "name": "Babysitter",
        "reference": "testing_localjson_connector_7",
        "summary": "Babysitter Job",
        "created_at": "2022-02-29T11:54:49",
        "location": {
            "text": "London United Kingdom"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Home Services"
            }
        ]
    }
]
```

</details>


### ✨🚀 Understanding `RunResult` ✨🚀
Running any `ConnectorAction` returns an instance of `RunResult`. It has the following three attributes :
- `status` :
    - `"success"` : When no error occured at any step of the run. All data that was `read` from the `origin` warehouse made its way to the `target` warehouse
    - `"fatal"` : When a critical error occured during the run. No data reached the `target` warehouse
    - `"success_with_failures"` : Some data _but not all_ was written to the `target` warehouse
- `reason` :
    - `""` _(empty string)_ : When `status` is `"success"` or `"success_with_failure"`
    - `"bad_action_parameters"` : The parameters given as `action_parameters` are not valid _e.g. you give an `int` as `format`_
    - `"bad_origin_parameters"` : The parameters given as `origin_parameters` are not valid _e.g. you omit a required parameter_
    - `"bad_target_parameters"` : The parameters given as `target_parameters` are not valid _e.g. you omit a required parameter_
    - `"format_failure"` : The `format` function raised an exception for **_all_** items
    - `"logics_failure"`: The `logics` chain of functions raised an exception for **_all_** items
    - `"read_failure"`: An exception occured during the `read` operation from the very beginning stopping the operation
    - `"write_failure"`: All items failed to be written to the `target` warehouse
    - `"workflow_id_not_found"`: This is one of the possible initialization errors that can happen when using `hrflow_connectors` in the Cloud on HrFlow.ai plateform
    - `"event_parsing_failure"`: This is one the possible initialization errors that can happen when using `hrflow_connectors` in the Cloud on HrFlow.ai plateform. It happens when the logic you defined to parse the hook triggering your action fails unexpectedly
    - `"backend_not_configured_in_incremental_mode"`: `read_mode` is `ReadMode.incremental` but no backend was configured
    - `"origin_does_not_support_incremental"`: Happens when `read_mode` is `ReadMode.incremental` but the origin warehouse does not support that mode
    - `"item_to_read_from_failure"` : The action failed when trying to get the `read_from` flag from the origin warehouse
- `events` : A counter with the counts for the following events `"read_success"` `"read_failure"` `"format_failure"` `"logics_discard"` `"logics_failure"` `"write_failure"` `"callback_executed"` `"callback_failure"` and `"item_to_read_from_failure"`
- `read_from` : A  string identifier returned by the origin warehouse when `read_mode=ReadMode.incremental`. During the next execution of the action that value will be given to the `read` function. Depending on implementation it can be used to only _read_ newer items or only beyond a certain limit and avoid pulling already visited items


## Testing
In order to ensure a minimal level of quality warehouses and connectors are expected to be automatically tested against a set of basic scenarios. To make this as simple as possible a YAML based testing framework is in place.

For each connector you should find a `test-config.yaml` file to configure integration tests. The file should be in the connector's directory at the same level as `warehouse.py` or `connector.py`.

Tests for both connector actions and warehouses can be defined.

### Testing a `Warehouse`
:warning: _For now only `read` operation is supported by the testing framework_ :warning:

For each `read` test case you must supply :
- `parameters` : A dictionnary with parameters that will be passed to the `read` function of the `Warehouse`
- **[Optional]** `read_mode` : One of `sync` or `incremental` 
- **[Optional]** `read_from` : A string identifier passed to the `read` function of the `Warehouse`
- **[Optional]** `expected_number_of_items` : If provided the test will pass only if the `read` operation returns that exact number of items

A `read` test passes :heavy_check_mark: if :
- the `read` operation with the provided `parameters` does not raise any exception
- the `read` operation returns at least one item or exactly `expected_number_of_items` if it's defined

To illustrate let's consider how we could achieve the following test cases :
- We want to test `LocalJSONWarehouse`
- We want to make sure that for a simple JSON file with three items the `read` operation returns that number of items
- We want to make sure that for an empty JSON the `read` operation returns no items

This can be achieved by supplying the following `test-config.yaml` file.

```yaml
warehouse:
  # This should be the exact Warehouse instance name as defined in the connector's **warehouse.py**
  LocalJSONWarehouse:
    read:
      - id: empty_json
        parameters:
          path: tests/data/localjson/empty.json
        expected_number_of_items: 0
      - parameters:
          path: tests/data/localjson/fruits.json
        expected_number_of_items: 3
# This tests is added only for illustration purposes to show
# that expected_number_of_items is not mandatory
      - id: same_as_previous_but_without_expected_number_of_items
        parameters:
          path: tests/data/localjson/fruits.json
# This tests should fail because the provided JSON file is empty
# when expected_number_of_items is not defined at least one item
# must be returned
      - id: this_should_fail
        parameters:
          path: tests/data/localjson/empty.json

```

Before running add both files under [`tests/data/localjson`](./tests/data/localjson/)
- `fruits.json`
```JSON
[
    {"id": 0, "name": "tomato"},
    {"id": 1, "name": "orange"},
    {"id": 2, "name": "apple"}
]
```
- `empty.json`
```JSON
[]
```

Then run `poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON`.

_Make sure that you properly added `LocalJSON` to `__CONNECTORS__` in [`src/hrflow_connectors/__init__.py`](src/hrflow_connectors/__init__.py) before running the command_.

You should be able to see the three first tests pass and the last failing as expected. You can also see how the output is different when you specify the optional parameter `id` in `test-config.yaml`

### Testing a `ConnectorAction`

For each `ConnectorAction` test case you must supply :
- `origin_parameters` : This maps to the `origin_parameters` used to invoke the action
- `target_parameters` : This maps to the `target_parameters` used to invoke the action
- **[Optional]** `id` : A string to name the particular test case. This makes debugging and reading tests' results easier
- **[Optional]** `status` : If provided the returned `RunResult` should have that exact `status` value. See this [section](#✨🚀-understanding-runresult-✨🚀) for more about `RunResult`
- **[Optional]** `reason` : If provided the returned `RunResult` should have that exact `reason` value. See this [section](#✨🚀-understanding-runresult-✨🚀) for more about `RunResult`
- **[Optional]** `events` : If provided the returned `RunResult` should have the same event counts. See this [section](#✨🚀-understanding-runresult-✨🚀) for more about `RunResult`


Once again to illustrate let's consider the following test cases :
- We want to test the `pull_jobs` action of `LocalJSON`
- We want to make sure that if we forget to give `path` in `origin_parameters` we have `status="fatal"`
- We want to make sure that if we point `path` to a file which is not valid JSON we have `status="fatal"` and `reason="read_failure"`
- We want to make sure that with a fake `api_secret` in `target_parameters` we have `status="fatal"`, `reason="write_failure"` and `events` counts showing that all succeded except the `writing_failure`

This can be achieved by supplying the following `test-config.yaml` file.

```yaml
actions:
  # This should the exact ConnectorAction.name
  pull_jobs:
    - id: missing_path
      origin_parameters: {}
      target_parameters:
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
    - id: bad_json_input
      origin_parameters:
        path: tests/data/localjson/bad_json.json
      target_parameters:
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
      reason: read_failure
    - origin_parameters:
        path: tests/data/localjson/valid.json
      target_parameters:
        # Obviously this is not a valid api_secret
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
      reason: write_failure
      events:
        read_success: 2
        read_failure: 0
        write_failure: 2
```

Before running add both files under [`tests/data/localjson`](./tests/data/localjson/)
`bad_json.json`
```json
1 + {}
```

`valid.json`
```json
[
    {
        "name": "Accounting assistant",
        "reference": "testing_localjson_connector_1",
        "summary": "Accounting assistant Job",
        "created_at": "2022-01-22T11:54:49",
        "location": {
            "text": "Paris France"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "Services"
            }
        ]
    },
    {
        "name": "Software engineer",
        "reference": "testing_localjson_connector_2",
        "summary": "Software engineer Job",
        "created_at": "2022-03-22T11:54:49",
        "location": {
            "text": "Tokyo Japan"
        },
        "sections": [],
        "tags": [
            {
                "name": "Category",
                "value": "IT"
            }
        ]
    }
]
```

Then run `poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON`.

_Make sure that you properly added `LocalJSON` to `__CONNECTORS__` in [`src/hrflow_connectors/__init__.py`](src/hrflow_connectors/__init__.py) before running the command_.

<details>
<summary>Complete <code>test-config.yaml</code> with <code>Warehouse</code> and <code>ConnectorAction</code> tests </summary>


```yaml
warehouse:
  LocalJSONWarehouse:
    read:
      - id: empty_json
        parameters:
          path: tests/data/localjson/empty.json
        expected_number_of_items: 0
      - id: fruits json
        parameters:
          path: tests/data/localjson/fruits.json
        expected_number_of_items: 3

actions:
  pull_jobs:
    - id: missing_path
      origin_parameters: {}
      target_parameters:
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
    - id: bad_json_input
      origin_parameters:
        path: tests/data/localjson/bad_json.json
      target_parameters:
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
      reason: read_failure
    - origin_parameters:
        path: tests/data/localjson/valid.json
      target_parameters:
        # Obviously this is not a valid api_secret
        api_secret: xxxxAPIUSER
        api_user: user@hrflow.ai
        board_key: xxxxMyBoardKey
      status: fatal
      reason: write_failure
      events:
        read_success: 2
        read_failure: 0
        write_failure: 2
```

</details>

### Using **secrets** in tests

You might need to provide secret parameters in `test-config.yaml`. To do so prepend the corresponding parameter value with **`$__`** then add the _name of the secret_.

To illustrate let's consider this simple `Warehouse` test where we treat the `path` _parameter_ as a secret:

`test-config.yaml`
```yaml
warehouse:
  LocalJSONWarehouse:
    read:
      - id: empty_json
        parameters:
          path: $__SECRET_JSON_PATH
        expected_number_of_items: 0
```

_Make sure to add `empty.json` under [`tests/data/localjson`](./tests/data/localjson/)_

```json
[]
```

You can try now to run tests for `LocalJSON` and confirm that it fails because it doesn't find the value of `SECRET_JSON_PATH`.

```bash
poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON
```

Secrets are fetched from three locations with this order of precedence :
- Environment variables : This naming convention should be respected `HRFLOW_CONNECTORS_{CONNECTOR_NAME_UPPERCASED}_{SECRET_NAME}`
```bash
HRFLOW_CONNECTORS_LOCALJSON_SECRET_JSON_PATH=tests/data/localjson/empty.json poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON
```
- Connector's secrets : The secrets should be written to a JSON file named `secrets.json` under the connector's directory at the same level as `warehouse.py` or `connector.py`

Add `secrets.json` under [`src/hrflow_connectors/connectors/localjson`](src/hrflow_connectors/connectors/localjson/)
```json
{
    "SECRET_JSON_PATH": "tests/data/localjson/empty.json"
}
```
Then run
```bash
poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON
```

- Global secrets file : The secrets should be written to a JSON file named `secrets.json` under [`src/hrflow_connectors/connectors/`](src/hrflow_connectors/connectors/). The naming convention is the same as for environment variables `HRFLOW_CONNECTORS_{CONNECTOR_NAME_UPPERCASED}_{SECRET_NAME}`. _Don't forget to remove the `secrets.json` file created above before testing_

Add `secrets.json` under [`src/hrflow_connectors/connectors/`](src/hrflow_connectors/connectors/)
```json
{
    "HRFLOW_CONNECTORS_LOCALJSON_SECRET_JSON_PATH": "tests/data/localjson/empty.json"
}
```
Then run
```bash
poetry run pytest --no-cov --ignore tests/core --connector=LocalJSON
```

## Advanced topics
### Mutating a `Warehouse` to fix some parameters

Let's consider the case of `LocalJSON` connector with a single action that reads profiles from `HrFlowProfileWarehouse` and writes them to a JSON file.

```python
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.localjson.warehouse import LocalJSONWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)

LocalJSON = Connector(
    name="LocalJSON",
    description="Read from JSON, Write to JSON",
    url="https://localjson.ai",
    actions=[
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description="Push a profile from a Hrflow.ai Source to a local JSON file",
            parameters=BaseActionParameters,
            origin=HrFlowProfileWarehouse,
            target=LocalJSONWarehouse,
        ),
    ],
)
```

When the end user will eventually use the `push_profile` action defined above he will need to provide the parameters expected by `HrFlowProfileWarehouse`'s `read` function.

If some of these parameters should be **fixed** at the `ConnectorAction` level and not _left for the user to choose_ you can use one of `with_fixed_read_parameters` or `with_fixed_write_parameters` `Warehouse`'s methods to achieve the desired behavior.

Below are a few examples :
- `api_user` should always be `"from_localjson_connector@hrflow.ai"`
```python
LocalJSON = Connector(
    name="LocalJSON",
    description="Read from JSON, Write to JSON",
    url="https://localjson.ai",
    actions=[
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description="Push a profile from a Hrflow.ai Source to a local JSON file",
            parameters=BaseActionParameters,
            origin=HrFlowProfileWarehouse.with_fixed_read_parameters(
                api_user="from_localjson_connector@hrflow.ai"
            ),
            target=LocalJSONWarehouse,
        ),
    ],
)
```
- `api_user` resp. `source_key` should always be `"from_localjson_connector@hrflow.ai"` resp. `"some_fixed_source_key"`
```python
...
    origin=HrFlowProfileWarehouse.with_fixed_read_parameters(
        api_user="from_localjson_connector",
        source_key="some_fixed_source_key"
    )
...
```

### Addind a callback to your `ConnectorAction`
It is possible to add a callback function when developping a new `ConnectorAction`. 

The callback signature should be as follow:
```python
def my_callback_function(
    origin_parameters: BaseModel, # <==== parameters that were given to the origin warehouse
    target_parameters: BaseModel, # <==== parameters that were given to the target warehouse
    events: t.Counter[Event], # <==== Counter of events that occured during the execution
    items_to_write: t.List[t.Dict], # <==== The list of items that the action tried to write to the target
) -> None:
    # Any code logic

my_action_with_callback = ConnectorAction(
    name=...,
    trigger_type=...,
    description=...,
    parameters=...,
    origin=...,
    target=...,
    callback=my_callback_function,
)  
```
You are free to use any or none of the given arguments. But if your callback depends on the events that might have occured during execution then you should have enough data to build your custom logic. 

The callback function is called _after the write operation_ **but only if the execution didn't exit early**. 
To know if your callback was executed or not you should check the returned `RunResult` and look for `Event.callback_executed` and/or `Event.callback_failure`. 
> For example if the `origin_parameters` are not valid the execution of the action is stopped early and the callback function is not called. 

### How to do _incremental_ reading
#### Concepts
One common use case for connectors is to maintain two different warehouses synced over time. Using the `trigger_type=WorkflowType.pull` parameter of `ConnectorAction` it is possible to setup on the HrFlow.ai plateform a recurring action. Every given period of time the action is executed making sure that items from the origin are written to the target. 

But in such cases pulling all items from the origin each time can become very time consuming on top of being inefficient. For this reason there is a way to implement _incremental_ reading within `hrflow_connectors`. 

The main idea is that after each run of the action a string flag named `read_from` is returned by the origin warehouse. During the next execution of the action that `read_from` flag is given to the `read` function allowing it to only fetch relevent items given the flag value. 

One simple example can be to set `read_from` to the execution time of the action. During the next iteration the `read` function of the origin warehouse can use the value stored in `read_from` to only fetch items that have a `created_at` or `updated_at` greater than the last `read_from`. 

> **_Mind_** that what `read_from` should be and how it will be used is completely implementation dependent. Each warehouse depending on its internal logic can have a different logic for `read_from`. 

#### Backend
In order for the _incremental_ flow to work a **backend** needs to be configured. The backend is a store which is used in order to persist the value of `read_from`. That way it can be fetched and given to the action during future executions. 

> :warning: If you try to run an action in _incremental_ mode without configuring a backend you will get a failure with `reason=backend_not_configured_in_incremental_mode`.

> For steps on how to configure a backend you can check the [dedicated section](#backend-configuration)

#### Interface
In order for a warehouse to support _incremental_ reading it needs to implement the following interface:
- The optional `item_to_read_from` callable of a `WarehouseReadAction` must be provided. That callable should accept a single argument and return the `read_from` string flag. That single argument is the last item returned by the `read` operation. It is up to the logic in `item_to_read_from` to use or not that argument in order to compute `read_from`
- The optional `supports_incremental` boolean flag of a `WarehouseReadAction` must be set to `True`

#### Example
In order to illustrate how to use the _incremental_ mode we will consider the following use case : 
- We have a warehouse that fetches _orders_ from a SQLite table
- Each _order_ has an `updated_at` field which is stored as an integer unix timestamp
- The warehouse always returns _orders_ ordered by `updated_at` in an ascending fashion 

Let's consider how it can be implemented without _incremental_ logic. 

```python
import sqlite3
import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel, Field

from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction

class ReadOrderParameters(BaseModel):
    db_name: str = Field(..., repr=False)

def read(
    adapter: LoggerAdapter,
    parameters: ReadOrderParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    connection = sqlite3.connect(parameters.db_name)
    try:
        cursor = connection.cursor()
        cursor.execute("SELECT id, product, quantity, updated_at FROM orders ORDER BY updated_at ASC;")
        for order in cursor:
            yield dict(id=order[0], product=order[1], quantity=order[2], updated_at=order[3])
    finally:
        cursor.close()
        connection.close()
        

OrdersWarehouse = Warehouse(
    name="OrdersWarehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadOrderParameters,
        function=read,
    ),
)
```
In this implementation for each execution all orders are fetched. The `read` function doesn't make use of `read_mode` nor `read_from`. 

🧪 Now let's see how the implementation can be made _incremental_. We will make the following changes to the code : 
- We will assume that `read_from` when provided is the `update_at` of the last order that was read during a previous execution of the action
- We will add a simple `item_to_read_from` callable that given an order returns `order["updated_at"]` 
- We will change the query sent to MySQL in the case where `read_mode=ReadMode.incremental` in order to only fetch relevent items

```python
def read(
    adapter: LoggerAdapter,
    parameters: ReadOrderParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    connection = sqlite3.connect(parameters.db_name)
    query = (
        "SELECT id, product, quantity, updated_at FROM orders ORDER BY updated_at ASC;"
    )
    if read_mode is ReadMode.incremental and read_from is not None:
        # Making use of the last updated_at that was read in order to only fetch new items not already written to the target Warehouse
        query = (
            "SELECT id, product, quantity, updated_at FROM orders WHERE updated_at > {}"
            " ORDER BY updated_at ASC;".format(read_from)
        )
    try:
        cursor = connection.cursor()
        cursor.execute(query)
        for order in cursor:
            yield dict(
                id=order[0], product=order[1], quantity=order[2], updated_at=order[3]
            )
    finally:
        cursor.close()
        connection.close()


SQLiteOrdersWarehouse = Warehouse(
    name="SQLiteOrdersWarehouse",
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadOrderParameters,
        function=read,
        # Set the boolean flag to True
        supports_incremental=True,
        item_to_read_from=lambda order: order["updated_at"],
    ),
)
```

#### Running the action
With the concepts and example code at hand let's put all that theory into practice.
1. Create a new folder `sqliteorders` under [`src/hrflow_connectors/connectors/`](src/hrflow_connectors/connectors/).
2. Put following content in `warehouse.py` under [`src/hrflow_connectors/connectors/sqliteorders`](src/hrflow_connectors/connectors/sqliteorders)  
    <details>
    <summary><code>warehouse.py</code></summary>

    ```python
    import sqlite3
    import typing as t
    from logging import LoggerAdapter

    from pydantic import BaseModel, Field

    from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction


    class ReadOrderParameters(BaseModel):
        db_name: str = Field(..., repr=False)


    def read(
        adapter: LoggerAdapter,
        parameters: ReadOrderParameters,
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
        connection = sqlite3.connect(parameters.db_name)
        query = (
            "SELECT id, product, quantity, updated_at FROM orders ORDER BY updated_at ASC;"
        )
        if read_mode is ReadMode.incremental and read_from is not None:
            # Making use of the last updated_at that was read in order to only fetch new items not already written to the target Warehouse
            query = (
                "SELECT id, product, quantity, updated_at FROM orders WHERE updated_at > {}"
                " ORDER BY updated_at ASC;".format(read_from)
            )
        try:
            cursor = connection.cursor()
            cursor.execute(query)
            for order in cursor:
                yield dict(
                    id=order[0], product=order[1], quantity=order[2], updated_at=order[3]
                )
        finally:
            cursor.close()
            connection.close()


    SQLiteOrdersWarehouse = Warehouse(
        name="SQLiteOrdersWarehouse",
        data_type=DataType.other,
        read=WarehouseReadAction(
            parameters=ReadOrderParameters,
            function=read,
            # Set the boolean flag to True
            supports_incremental=True,
            item_to_read_from=lambda order: order["updated_at"],
        ),
    )
    ```
    </details>

3. Put following content in `connector.py` under [`src/hrflow_connectors/connectors/sqliteorders`](src/hrflow_connectors/connectors/sqliteorders). We are using `SQLiteOrders` in conjonction with `LocalJSON` from the tutorial
    <details>
    <summary><code>connector.py</code></summary>

    ```python
    from hrflow_connectors.connectors.localjson.warehouse import LocalJSONWarehouse
    from hrflow_connectors.connectors.sqliteorders.warehouse import SQLiteOrdersWarehouse
    from hrflow_connectors.core import (
        BaseActionParameters,
        Connector,
        ConnectorAction,
        WorkflowType,
    )

    SQLiteOrders = Connector(
        name="SQLiteOrders",
        description="Read from SQLite, Write to JSON",
        url="https://sqliteorder.ai",
        actions=[
            ConnectorAction(
                name="pull_orders",
                trigger_type=WorkflowType.pull,
                description="Send orders from SQLite to JSON file.",
                parameters=BaseActionParameters,
                origin=SQLiteOrdersWarehouse,
                target=LocalJSONWarehouse,
            ),
        ],
    )
    ```

    </details>

4. Add `SQLiteOrders` to the `__CONNECTORS__` list in [`src/hrflow_connectors/__init__.py`](src/hrflow_connectors/__init__.py)
5. Make sure that everything is okay by generating the docs `make docs`. You should see instructions about how to use the `pull_orders` action in [`src/hrflow_connectors/connectors/sqliteorders/docs/pull_orders.md`](src/hrflow_connectors/connectors/sqliteorders/docs/pull_orders.md)
6. Configure `LocalJSONStore` backend by setting proper environment variables in your terminal as described [here](#backend-configuration) and [here](#localjsonstore)
    ```bash
    export HRFLOW_CONNECTORS_STORE_ENABLED=True
    export HRFLOW_CONNECTORS_STORE=localjson
    export HRFLOW_CONNECTORS_LOCALJSON_DIR=/tmp/
    ```
7. Open an iPython terminal with `hrflow_connectors` scope by running `make ipython` in the same terminal where you configured the previous environment variables
8. Run the following instructions

    ```python
    import logging
    from hrflow_connectors import SQLiteOrders
    from hrflow_connectors.core import ReadMode

    logging.basicConfig(level=logging.INFO)

    # This first run should fail because no table orders exists in tutorial.db
    # Run the action then check that :
    # - status == Status.fatal
    # - reason == Reason.read_failure
    # - result is stored in /tmp/store.json under `testing_incremental` 
    # - target_warehouse.json is empty
    result = SQLiteOrders.pull_orders(
        workflow_id="testing_incremental",
        action_parameters=dict(
            read_mode=ReadMode.incremental,
        ),
        origin_parameters=dict(
            db_name="tutorial.db",
        ),
        target_parameters=dict(
            path="target_warehouse.json",
            mode="append"
        )
    )

    # Let's create the table
    import sqlite3
    con = sqlite3.connect("tutorial.db")
    cur = con.cursor()
    cur.execute("CREATE TABLE orders(id, product, quantity, updated_at)")

    # Run the action then check that :
    # - status == Status.success
    # - Event.read_success == 0
    # - result is stored in /tmp/store.json under `testing_incremental` 
    # - target_warehouse.json is empty
    result = SQLiteOrders.pull_orders(
        workflow_id="testing_incremental",
        action_parameters=dict(
            read_mode=ReadMode.incremental,
        ),
        origin_parameters=dict(
            db_name="tutorial.db",
        ),
        target_parameters=dict(
            path="target_warehouse.json",
            mode="append"
        )
    )

    # Let's add some orders
    data = [
        (34492, "pizza", 2, 1660000006), # 6
        (59683, "burger", 1, 1660000001), # 1
        (59285, "salad", 3, 1660000004), # 4
        (68483, "orange_juice", 1, 1660000002), # 2
        (98543, "pizza", 5, 1660000005), # 5
        (65345, "falafel", 3, 1660000003), # 3
    ]
    cur.executemany("INSERT INTO orders VALUES(?, ?, ?, ?)", data)
    con.commit()  

    # Run the action then check that :
    # - Event.read_success == 6
    # - target_warehouse.json has the six orders of the db in the right order
    # - 1660000006 which is the update_at of the last order is stored in /tmp/store.json under `testing_incremental` key as `read_from`
    result = SQLiteOrders.pull_orders(
        workflow_id="testing_incremental",
        action_parameters=dict(
            read_mode=ReadMode.incremental,
        ),
        origin_parameters=dict(
            db_name="tutorial.db",
        ),
        target_parameters=dict(
            path="target_warehouse.json",
            mode="append"
        )
    )

    # Now let's run another incremental run without adding order to the db
    # Since no new orders have been added nothing should be read from the origin
    # Run the action then check that :
    # - Event.read_success == 0
    # - target_warehouse.json is unchanded
    # - 1660000006 is stored in /tmp/store.json under `testing_incremental` key as `read_from`
    result = SQLiteOrders.pull_orders(
        workflow_id="testing_incremental",
        action_parameters=dict(
            read_mode=ReadMode.incremental,
        ),
        origin_parameters=dict(
            db_name="tutorial.db",
        ),
        target_parameters=dict(
            path="target_warehouse.json",
            mode="append"
        )
    )

    # Now let's add two new orders
    data = [
        (73958, "fish_and_ships", 1, 1660000008), # 8
        (35878, "lasagna", 1, 1660000007), # 7
    ]
    cur.executemany("INSERT INTO orders VALUES(?, ?, ?, ?)", data)
    con.commit()  

    # One final incremental run
    # Run the action then check that :
    # - Event.read_success == 2
    # - target_warehouse.json is has the new orders in the right order lasagne before fish_and_ships
    # - 1660000008 is stored in /tmp/store.json under `testing_incremental` key as `read_from`
    result = SQLiteOrders.pull_orders(
        workflow_id="testing_incremental",
        action_parameters=dict(
            read_mode=ReadMode.incremental,
        ),
        origin_parameters=dict(
            db_name="tutorial.db",
        ),
        target_parameters=dict(
            path="target_warehouse.json",
            mode="append"
        )
    )
    ```


## Backend Configuration
Configuration is based on environment variables that must be properly set during action execution: 
- `HRFLOW_CONNECTORS_STORE_ENABLED` : Any non empty string **different from** `"false"`, `"False"` or `"0"` **activates** the backend configuration
- `HRFLOW_CONNECTORS_STORE`: Set to the name of the store that you want to configure

### `LocalJSONStore`
- `HRFLOW_CONNECTORS_STORE`:  `"localjson"`
- `HRFLOW_CONNECTORS_LOCALJSON_DIR`: **[Required]** Absolute filepath to a directory where the JSON store will be written

### `S3Store`
- `HRFLOW_CONNECTORS_STORE`:  `"s3"`
- `HRFLOW_CONNECTORS_S3_BUCKET`: **[Required]** S3 bucket
- `HRFLOW_CONNECTORS_S3_PREFIX`: **[Optional]** S3 key prefix for all data stored by backend
- `HRFLOW_CONNECTORS_S3_AWS_REGION`: **[Required]** AWS region name
- `HRFLOW_CONNECTORS_S3_AWS_ACCESS_KEY_ID`: **[Required]** AWS ACCESS KEY ID
- `HRFLOW_CONNECTORS_S3_AWS_SECRET_ACCESS_KEY`: **[Required]** AWS SECRET ACCESS KEY