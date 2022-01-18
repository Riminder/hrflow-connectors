# Contributing

**Welcome to `hrflow-connectors` contributor's guide.**

We thank you for your interest in contributing to our open source hrflow-connectors. Contribution guidelines are listed below. If you don't know where or how to start contributing, we recommend reading the project documentation and test for yourself the already existing connectors to get a grasp of the logic behind them and how they work. We also recommend reading and familiarizing with the hrflow developers documentation `https://developers.hrflow.ai/reference/authentication`. We welcome any contributions from the community big or small.

# Code of conduct
Please notice, all users and contributors are expected to be **open,
considerate, reasonable, and respectful**. When in doubt, *Python Software
Foundation's Code of Conduct* is a good reference in terms of behavior
guidelines.


## Code Contributions

## Environment setup
1. Git clone: type  `git clone https://github.com/Riminder/hrflow-connectors.git` on your shell or `Clone Git Repository` in VSCode
2. `poetry install` in the repository shell to add all required dependencies to your virtual environment
It creates a virtual environment `.venv` at the root of the project with all necessary dependencies installed and even the `hrflow-connectors` package installed in editable mode.
3. `poetry shell` in the repository
This allows you to activate and use the virtual environment.
For more information about `poetry` and its usage, see the official documentation at `https://python-poetry.org/docs/`
Note: if you use another vitural environment tool like pip or others, you can add the required dependencies with their specific versions to a `requirements.txt` file and run `pip install -r requirements.txt` but we recommend using poetry, it is more practical and less confusing to use.

## Style recommendation for code readeablitiy
1. Use `black` to beautify and make your code more clearer and readable. In your shell, type black `actions.py` for exmaple.
2. Precise as much as you can the types of your arguments and the type of what your function returns, for example `f(x:int) -> None`.
3. Use docstrings in your functions with respect to the google convention.
4. avoid unneccessary line spacing

## Contributing to the project
Now, when your environment is set up, to test that everything is working properly use your testing tool to run the tests for core and utils modules functions or go to your terminal and run `pytest -s tests/core` use the same expression for utils by switching it with core.
There are several ways to contribute to the project, among others we state the following:
1. Adding a new connector: 
Add the connector module name for example `myconnector` in the connectors directory, make sure it respects the architecture specified in the ***documentation** file i.e: 
contains an `actions.py` file which contains all the actions your connector will implement(`PullJobs`, `PullProfile`, `PushJobs`, `PushProfile`...etc) -
each action is a class that inherits from its corresponding action parent class in the `action.py` `core` file for example `PullJobs` inherits from the class `PullJobsAction` and has as parameters the specific attributes to the connector action like `subdomain` for `urls` and so on and the auth parameter which gives AUthorization to the client API if it is required, and the auth parameter, you can see the auth available classes and methods in the `auth.py` `core` file, if you don't find there an Auth class that is compatible with your application, you can create a new one. You can also find in the utils module some interesting functions and debugging utilities that might help you with your contribution journey.
contains a `connector.py` file which contains your actions executed in a class `MyConnector` that inherits the abstract class Connector in the core module and for each action the class implements a static method for example for `PullJobs` action your method is named `pull_jobs` takes as parameters your connector required parameters and `**kwargs` for other optional arguments and logics.
contains a `__init__.py` file to make importing your connector easier from the package, you write in this file `from .connector import *`, don't also to forget to write ` from myconnector import * ` in the `__init__.py` file inside the `connectors` folder which is the parent to your connector folder.
contains a `schemas.py` file which describes the model of your action data, see corresponding files for other connectors to get your ideas starting.
and finally add a `README.md` file which contains a resume of your connector and list of action that points to each action documentation inside the folder you will create named `docs` in your connector folder. 



[^contrib1]: My reference. Even though, these resources focus on open source projects and communities, the general ideas behind collaborating with other developers to collectively create software are general and can be applied to all sorts of environments, including private companies and proprietary code bases.

---


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


## Documentation Improvements

You can help improve `hrflow-connectors` docs by making them more readable and coherent, or by adding missing information and correcting mistakes.

`hrflow-connectors` documentation uses Sphinx as its main documentation compiler.
This means that the docs are kept in the same repository as the project code, and
that any documentation update is done in the same way was a code contribution.

Please notice that the text documents are formatted in **Markdown**.

## Code Contributions
1. Git clone
2. `poetry install` in the repository
It creates a virtual environment `.venv` at the root of the project with all necessary dependencies installed and even the `hrflow-connectors` package installed in editable mode.
3. `poetry shell` in the repository
This allows you to activate and use the virtual environment.

[^contrib1]: My reference. Even though, these resources focus on open source projects and communities, the general ideas behind collaborating with other developers to collectively create software are general and can be applied to all sorts of environments, including private companies and proprietary code bases.

---