# 📖 Documentation
**This page shows you the different features shared between all connectors and how to use them.**

ℹ️ If you want to access the detailed documentation for a particular connector, go directly to the README for that connector.

## Logics
**`logics` are the set of logic functions that filter the data before synchronizing it. Thus, with `logics`, you can decide to process only certain elements of a stream.**

In the action execution pipeline, here is where the `logics` are located:
1. `pull` the data
2. **Apply `logics` functions to keep only certain elements**.
3. Apply `format` to each element
4. `push` the data

**All `Action`s have the following parameters:**
* `logics` which is a list of function names. This will be used to filter the elements returned by `pull`.
* `global_scope` is a dictionary containing the current scope's global variables.
* `local_scope` is a dictionary containing the current scope's local variables.
  
**The definition of a logical function:**
* The function takes only one parameter. This is an element of the stream.
* The function returns `bool` :
  * `True` indicates that the element is kept for the rest of the processing
  * `False` indicates that the element is not processed in the rest of the pipeline

`global_scope` and `local_scope` are only used to tell the connector how to `eval` the function names given in `logics`.
Most of the time, you would just write `global_scope=globals()` and `local_scope=locals()`.

ℹ️ To find out more about the scopes and how the `eval` function works: https://docs.python.org/3/library/functions.html#eval

### Example
```python
from hrflow_connectors.core.action import Action

def filter_element1_with_value1(element):
    return element.get("element1") == "value1"

action = Action(
    logics=["filter_element1_with_value1"],
    global_scope=globals(),
    local_scope=locals(),
)
```

Thus, if `pull` returns to step 1 :
```python
[
    dict(element1="value1", element2="value2"),
    dict(element1="value1", element2="value1"),
    dict(element1="value2", element2="value1"),
    dict(element1="value2", element2="value2")
]
```
After step 2, `format` and `push` will only deal with the following elements :
```python
[
    dict(element1="value1", element2="value2"),
    dict(element1="value1", element2="value1")
]
```
## Format
**The formatting of the data is the most important part of the connector. It indicates how to link two data streams.**

By default, all connectors have a `format` provided and ready to use. If you want to format the data for your particular use, you can redefine how the data is formatted when you connect.

**In the action execution pipeline, here is where the `format` is located:**
1. `pull` the data
2. Apply `logics` functions to keep only certain elements.
3. **Apply `format` to each element**
4. `push` the data

**You can do this in 2 ways:**
* **By inheriting** from the connector and **overriding** the `format` function
* **By using a format function external** to the connector

### By inheriting & overriding `format`
```python
from hrflow_connectors.core.action import Action

class ActionWithMyFormat(Action)
    def format(data):
        # my format
        job = dict(my_ref=data["id"])
        return job

action = ActionWithMyFormat()
action.execute()
```
### By using a format function external

**All `Action`s have the following parameters:**
* `format_function_name` which is the function name. This will be used to format each element returned by `pull`.
* `global_scope` is a dictionary containing the current scope's global variables.
* `local_scope` is a dictionary containing the current scope's local variables.
  
**The definition of a logical function:**
* The function takes only one parameter. This is an element of the stream.
* The function returns data adapted to the input format of the push function and ready to be sent.

`global_scope` and `local_scope` are only used to tell the connector how to `eval` the function name given in `format_function_name`.
Most of the time, you would just write `global_scope=globals()` and `local_scope=locals()`.

ℹ️ To find out more about the scopes and how the `eval` function works: https://docs.python.org/3/library/functions.html#eval

```python
from hrflow_connectors.core.action import Action

def my_format(data):
    # my format
    job = dict(my_ref=data["id"])
    return job

action = Action(
    format_function_name="my_format",
    global_scope=globals(),
    local_scope=locals(),
)
action.execute()
```

## Using parsing to enrich a job
**All `Action` for connectors found in `connectors/boards` have an option to enrich a job with parsing.**

Parsing will parse all the text fields and extract the characteristics of the job and add them to the appropriate fields in ***Hrflow.ai***: `skills`, `languages`, `certifications`, `courses`, `tasks`, ...

**To enable parsing, simply turn on the option `hydrate_with_parsing`.**
```python
from hrflow_connectors.core.action import BoardAction

action = BoardAction(hydrate_with_parsing=True, ...)
action.execute()
```
## Automatically archive a job when it is deleted from the stream
**All `Action` for connectors found in `connectors/boards` have an `archive_deleted_jobs_from_stream` option to automatically archive a Board job when it is deleted from the stream.**

This can be useful for synchronizing a board with a job stream.
In this case, you only need to enable the `archive_deleted_jobs_from_stream=True` option.

But in some cases, you may only want to add jobs without worrying that the job is still present in the stream. In this case, you can simply disable the option `archive_deleted_jobs_from_stream=False`.

Here is how it looks in an example :
```python
from hrflow_connectors.core.action import BoardAction

action = BoardAction(archive_deleted_jobs_from_stream=True, ...)
action.execute()
```

## Using `hrflow_connector` in a CATCH workflow to get a profile
**When you set up a [*CATCH Workflow*](https://developers.hrflow.ai/docs/workflows#catch-setup) triggered by a webhook** each time a profile is indexed (following the parsing of a CV for example), **you want to use a connector from `hrflow_connectors`.**

In `hrflow_connectors.utils.hrflow` you will find the `EventParser` class. This object takes care of reading the `_request` parameter of `workflow` and giving you the relevant information.

One method is of particular interest here: `get_profile`.
This method retrieves information from a profile if it exists in `_request`.

**Let's take a simple example:**
```python
from hrflow_connectors.connectors.core.action import ProfileDestinationAction
from hrflow_connectors.utils.hrflow import EventParser

def workflow(_request, settings):
    """
    CATCH Workflow
    """
    event = EventParser(request=_request)
    profile = event.get_profile()
    if profile is not None:
        action = ProfileDestinationAction(profile=profile, ...)
        response = action.execute()
        return response
```

If now you only want to process the profiles added **in the sources with the following keys `MY_SOURCE_KEY_1` and `MY_SOURCE_KEY_2`**.
Then you just have to write :
```python
from hrflow_connectors.connectors.core.action import ProfileDestinationAction
from hrflow_connectors.utils.hrflow import EventParser

def workflow(_request, settings):
    """
    CATCH Workflow
    """
    event = EventParser(request=_request)
    profile = event.get_profile(source_to_listen=["MY_SOURCE_KEY_1", "MY_SOURCE_KEY_2"])
    if profile is not None:
        action = ProfileDestinationAction(profile=profile, ...)
        response = action.execute()
        return response
```
So **if a profile is added to the source with the key `OTHER_SOURCE_KEY`** then `get_profile` would return `None` and **that profile would be ignored** from the rest of the processing.

## Logger
**All connectors use logging. The default `Handler` is `NullHandler`.**

To retrieve the logger, simply use `hrflow_connectors.utils.logger`.

We provide two functions:
* `get_logger()` **to retrieve only the logger**
* `get_logger_with_basic_config()` **to retrieve the logger with a default configuration** (displayed on std with defined formatting). This is an out-of-the-box solution for anyone who doesn't want to spend time configuring the logging and retrieving the logs directly from the console. The simple call to `get_logger_with_basic_config` will apply the configuration to the package logger and return it to you. ⚠️ **So call it multiple times, apply the config multiple times.**
  
```python
from hrflow_connectors.utils.logger import get_logger, get_logger_with_basic_config

# Calling only once
logger = get_logger_with_basic_config()

logger.info("Success")
logger.warning("Ah")

# OR
# You can call it as many times as you need
logger = get_logger()

logger.info("Ok")
logger.error("Super message")
```