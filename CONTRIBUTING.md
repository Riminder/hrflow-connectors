# Contributing

**Welcome to `hrflow-connectors` contributor's guide.**

We thank you for your interest in contributing to our open source project [hrflow-connectors](https://github.com/Riminder/hrflow-connectors). Contribution guidelines are listed below. If you don't know where or how to start contributing, we recommend reading the project [documentation](https://github.com/Riminder/hrflow-connectors/blob/master/DOCUMENTATION.md) and test for yourself the already existing connectors to get a grasp of the logic behind them and how they work. We also recommend reading and familiarizing with the [hrflow developers documentation](https://developers.hrflow.ai/reference/authentication). We welcome any contributions from the community big or small.

## Code of conduct
Please notice, all users and contributors are expected to be **open,
considerate, reasonable, and respectful**. When in doubt, *Python Software
Foundation's Code of Conduct* is a good reference in terms of behavior
guidelines.


## Code Contributions

### Environment setup
1. Git clone: type  `git clone https://github.com/Riminder/hrflow-connectors.git` on your shell or `Clone Git Repository` in VSCode
2. `poetry install` in the repository shell to add all required dependencies in [pyproject.toml](https://github.com/Riminder/hrflow-connectors/blob/master/pyproject.toml) to your virtual environment
It creates a virtual environment `.venv` at the root of the project with all necessary dependencies installed and even the `hrflow-connectors` package installed in editable mode.
3. `poetry shell` in the repository
This allows you to activate and use the virtual environment.
For more information about `poetry` and its usage, see the official documentation at `https://python-poetry.org/docs/`
Note: if you use another vitural environment tool like pip or others, you can add the required dependencies with their specific versions to a `requirements.txt` file and run `pip install -r requirements.txt` but we recommend using poetry, it is more practical and less confusing to use.


## Contributing to *hrflow-connectors*
Now, when your environment is set up, to test that everything is working properly use your testing tool to run the [tests](https://github.com/Riminder/hrflow-connectors/tree/master/tests) for core and utils modules functions or go to your terminal and run `pytest -s tests/core` use the same expression for utils by switching it with core.
There are several ways to contribute to the project, among others we state the following:

### Explicit Code Contributions
1. Building a new connector:
 
    - Add the connector module name for example `myconnector` in the [connectors](https://github.com/Riminder/hrflow-connectors/tree/master/tests/connectors) directory, make sure it respects the architecture specified in the [**DOCUMENTATION.md**](https://github.com/Riminder/hrflow-connectors/blob/master/DOCUMENTATION.md) file i.e: 

    - Contains an `actions.py` file which contains all the actions your connector will implement(`PullJobsAction`, `PullProfileAction`, `PushJobsAction`, `PushProfileAction`,...etc)

    - Each action is a class that inherits from its corresponding action parent class in the [action.py](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/core/action.py) `core` file for example `PullJobsAction` inherits from the class `PullJobsBaseAction` and has as parameters the specific attributes to the connector action like `subdomain` for `urls` and so on and the auth parameter which gives AUthorization to the client API if it is required, and the auth parameter, you can see the auth available classes and methods in the [auth.py](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/core/auth.py) `core` file, if you don't find there an Auth class that is compatible with your application, you can create a new one. You can also find in the utils module some interesting functions and debugging utilities that might help you with your contribution journey.

    - Contains a `connector.py` file which contains your actions executed in a class `MyConnector` that inherits the abstract class [Connector](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/core/connector.py) in the [core](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/core) module and for each action the class implements a static method for example for `PullJobsAction` action your method is named `pull_jobs` takes as parameters your connector required parameters and `**kwargs` for other optional arguments and logics.

    - Contains a `__init__.py` file to make importing your connector easier from the package, you write in this file `from .connector import *`, don't also forget to write `from myconnector import *` in the [__init__.py](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/connectors/__init__.py) file inside the [connectors](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors) folder which is the parent to your connector folder.

    - Contains a `schemas.py` file which describes the model of your action data, see corresponding files for other connectors to get your ideas starting. Here is a brief example:
    ```Python
    from pydantic import BaseModel, Field
    from typing import Dict, Optional, List, Any

    class MyDataModel(BaseModel):

        id: str=Field(..., description="My data id")
        name: str=Field(..., description="My data name")
        WorkRemote: Optional[bool]=Field(False, description="Indicates if work is done remotely or not, default is false")
        and_so_on: Any=Field(..., description="Other parameters and models of your data model")
    ```

    - Finally add a `README.md` file which contains a resume of your connector and list of action that points to each action documentation inside the folder you will create a folder `docs` in your connector folder.


2. Maintaining a connector:

    - You can also edit and improve the performance of an existing connector, track new bugs and correct them and most importantly propose a more optimal and elegant alternative.

3. Editing and Creating in [core](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/core) and [utils](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/utils) of the project:

    - We encourage contributors to add util functions and methods that use has been proven unnescapable in creating most of their connectors to the [core](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/core) and [utils](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/utils) folders and files.


### Coding style and performance

We encourage that you write your code in the simplest and most readable way possible so that anyone can understand it and test it especially the package users, your viewers and more importantly yourself, the following coding behaviour helps you do that:

- Avoid putting too many operations in one line, it doesn't optimize your code and makes it more complex to read and harder to find bugs origin.

- Use [pydantic](https://pydantic-docs.helpmanual.io/) for your data validation and settings management, it increases performance rapidly, makes sure there is not a type error or a validation error and points right to the line where the error is coming from which makes it easy to correct bugs. We recommend reading its documentation and we give here an easy example to see and grasp:
     ```python
        from pydantic import BaseModel, Field
        from typing import Dict, Any, Iterator, Optional
        
        class MyModel(BaseModel):
            number: int=Field(5,description="A number, easy right?")
            string: Optional[str]=Field(None, description="Can be any string and can be None")
            reference: str
            name = 'John Does'
            
        john_doe = MyModel()
        print(john_doe)
        #> ValidationError: 1 validation error for MyModel reference, 
        #field required (type=balue_error.missing)
        """Pydantic points to me that I missed to precise a required parameter `reference` which is mandatory and doesn't have a default value.
        However, there are no problems with other parameters, number already has an existing default value Field, if I don't want to precise a default value, I need to just write number: int=Field(..., description="a number, easy right?")
        String is an optional parameter so I am not forced to precise it but i can use it if needed and name is already a constant parameter so if I precise
        print(john_doe(name="John Doe", reference='a')) a validation error will also be raised
        """
        # note that string here can take any type
        # number can be only an int other a validation and type error are raise
        # same with reference           
     ```
- Use snake_case for your variables and CamelCase for your classes, make sure every parameter and function name corresponds to its basic usage.

- Avoid unnecessary imports and unnecessary operations and functions.

- Use only the package dependecies which you can find on the [pyproject.toml](https://github.com/Riminder/hrflow-connectors/blob/master/pyproject.toml) file and their dependencies and evidently you are free to use any python builtin module if needed.

- Always reformat your scripts with `black`, the uncompromised Python code formatter.

- Make sure that you test every function and action you create, there is a [tests](https://github.com/Riminder/hrflow-connectors/tree/master/tests) folder in the repo make sure you assign your test to the correct subfolder or file.

- Install `Coverage Gutters` in vscode to display test coverage information.

- Use logging in your code if you want to see what your code does in real time: 
   ```python 
    from ....utils.logger import get_logger
    logger = get_logger()
    ```
- We use pytest in the terminal to run tests: 
        `pytest -s tests\path_to_scripts_i_want_to_test`

- Write a # TODO comment for features that aren't implemented and that you think would improve performance and quality so that other contributors may be aware of them and may have solutions to them.

- For connectors, we recommend getting inspired from other [connectors](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors) and [tests](https://github.com/Riminder/hrflow-connectors/tree/master/tests) as it won't differ too much unless you propose a more optimal and elegant alternative.

- Always keep in mind that we don't want the code to use too much memory, so make sure if needed to use itertools and iterators in general to avoid huge memory consumption.

    - Use docstrings and type hints in your functions as much as you can, for example:
    ```python
    class MyConnector():        
        
        def pull(self) -> Iterator[Dict[str,Any]]:
            """
            Pulls all jobs from a specific endpoint
            
            Returns: list of all jobs pulled from specific endpoint 
            """
            operation
            operation
            .....
            
            return List_of_all_jobs
        
        def format(self, data:Dict[str,Any]) -> Dict[str, Any]:
            """
            Formats a job in the specific format
            
            Args data:Dict[str, Any]: a job in a specific 
            from the list of jobs 
            pulled in `pull` function
            
            Returns the job in the specific format
            """
            
            job = dict()
            
            # formatting attributes of the data to the specific format 
            # this is just an explanation, elaborate as you see fit.
            
            job[key] = formatted_data[key]
            #.... and so on
            
            return job
    ```
- Finally, we recommend reading, understanding and testing at least one of the [connectors](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors) and all its relative tests and docs and imports, it is a facilitating factor for building and implementing connectors and making them more performant and elegant.


### Commit conventions and review process

- Make sur your commits are periodical and each commit points to a specific modification of feature and follow the commit type style that follows:
    - for adding a new feature, or editing a feature outcome: `feat: add function format in actions.py`
    - for editing without changing outcome : `refacto:.....`, for style: `style: reformat with black/add docstrings...`
    - for testing: `test:.....`, for documenting: `doc:.....`, for bugs and fix: `fix...`
    - And so on, and so on

- If you consider contributing to the connector, as soon as you are done with your development, just put up a PR on github with a description of what the merge will do to the package and what will it improve or add, choose a reviewer because no PR can be merged without being reviewed, the reviewer should take into consideration the following:
    - Is your solution reasonable? and does it respect the conventions listed above?.
    - Is it tested? (unit tests)
    - Is it introducing security risks ?

- If there are requested changes from the reviewer, you must consider them before your PR can be merged and rerequest review after your modifications. Once approved, your PR will be merged and your name added to the authors.

## Issue Reports

If you experience bugs or general issues with `hrflow-connectors`, please have a look
on the `issue tracker`. If you don't see anything useful there, please feel
free to fire an issue report.

ℹ️ Please don't forget to include the closed issues in your search. Sometimes a solution was already reported, and the problem is considered **solved**.

New issue reports should include information about your programming environment
(e.g., operating system, Python version) and steps to reproduce the problem.
Please try also to simplify the reproduction steps to a very minimal example
that still illustrates the problem you are facing. By removing other factors,
you help us to identify the root cause of the issue.         