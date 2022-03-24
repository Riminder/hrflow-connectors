# 📖 Documentation
## Introduction to HrFlow.ai Connectors' framework

The framework is based on three main concepts : `Warehouse`, `ConnectorAction` and `Connector` :
- `Warehouse` is conceptually any source of data that you can either `read` from and/or `write` to
- `ConnectorAction` is an abstraction on top of two warehouses. An `origin` warehouse where data will be read from and a `target` warehouse where data will be written to
- `Connector` is an abstraction to group many `ConnectorAction`s

When contributing a new `Connector` you mainly have to define the corresponding `Warehouse`. You are bringing the knowledge of how to _read_ and/or _write_ to that new source of data. The rest is mainly boilerplate code and wire connecting with another already defined `Warehouse`.

## Prerequisites
Before you can develop new connectors or update existing ones you need to have `python` _3.8_ or above and [poetry](https://python-poetry.org/docs/) installed.

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
from hrflow_connectors.core import Warehouse, WarehouseReadAction, WarehouseWriteAction
```

To define a new `WarehouseReadAction` you need :
- A [pydantic](https://pydantic-docs.helpmanual.io/) schema that defines the arguments needed to read data from your warehoue. This is the place to define any necessary _tokens_ or _credentials_ for authentication but also any additional options or flags that can precise or scope the read operation. In our use case it's only the path to the JSON file to read from

```python
from pydantic import BaseModel, Field, FilePath


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")

```

- A callable that should accept two arguments: a  `LoggerAdapter` instance that should be used for logging and an instance of the `pydantic` schema defined earlier. The callable should return an _iterable_ that yields data as Python dictionnaries.

```python
import json
import typing as t
from json import JSONDecodeError
from logging import LoggerAdapter

from pydantic import BaseModel, Field, FilePath


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")


def read(adapter: LoggerAdapter, parameters: ReadJsonParameters) -> t.Iterable[t.Dict]:
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
Defining the `WarehouseWriteAction` is in many aspects quite similar. Only the corresponding callable has a different signature
```python
import json
import typing as t
from logging import LoggerAdapter
from pathlib import Path

from pydantic import BaseModel, Field


class WriteJsonParameters(BaseModel):
    path: Path = Field(..., description="Path where to save JSON file")


def write(
    adapter: LoggerAdapter, parameters: WriteJsonParameters, items: t.Iterator[t.Dict]
) -> None:
    items = list(items)
    try:
        with open(parameters.path, "w") as f:
            json.dump(items, f)
    # More error handling can be added to cope with file permissions for example
    except TypeError as e:
        message = "Failed to JSON encode provided items with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)
```

The last step is defining the `Warehouse`.
```python
LocalJSONWarehouse = Warehouse(
    name="LocalJSONWarehouse",
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
_`Warehouse` has also a `data_schema` attribute that can receive a [pydantic](https://pydantic-docs.helpmanual.io/) schema. This is for use cases where your warehouse stores data with a specific schema. We didn't use it here because any valid JSON can be read or written using `LocalJSONWarehouse`._

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

from hrflow_connectors.core import Warehouse, WarehouseReadAction, WarehouseWriteAction


class ReadJsonParameters(BaseModel):
    path: FilePath = Field(..., description="Path to JSON file to read")


def read(adapter: LoggerAdapter, parameters: ReadJsonParameters) -> t.Iterable[t.Dict]:
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


def write(
    adapter: LoggerAdapter, parameters: WriteJsonParameters, items: t.Iterator[t.Dict]
) -> None:
    items = list(items)
    try:
        with open(parameters.path, "w") as f:
            json.dump(items, f)
    # More error handling can be added to cope with file permissions for example
    except TypeError as e:
        message = "Failed to JSON encode provided items with error {}".format(repr(e))
        adapter.error(message)
        raise Exception(message)


LocalJSONWarehouse = Warehouse(
    name="LocalJSONWarehouse",
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
- `push_profile` : Pulls a profile from a HrFlow.ai Profile Source and writes it down to a local JSON file for inspection

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
            type=WorkflowType.pull,
            description="Send jobs from local JSON file to a ***Hrflow.ai Board***.",
            parameters=BaseActionParameters,
            origin=LocalJSONWarehouse,
            target=HrFlowJobWarehouse,
        ),
        ConnectorAction(
            name="push_profile",
            type=WorkflowType.catch,
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
            type=WorkflowType.pull,
            description="Send jobs from local JSON file to a ***Hrflow.ai Board***.",
            parameters=BaseActionParameters.with_default_format(
                "PullJSONJobsActionParameters", format=format_job
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

##### `type`
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

When called each `ConnectorAction` should be supplied with three **mandatory** arguments :
- `action_parameters` : this is where you can supply `format` and `logics` functions
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


LocalJSON.pull_jobs(
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

</br>
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
</br>
