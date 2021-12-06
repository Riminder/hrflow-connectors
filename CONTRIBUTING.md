# Contributing

**Welcome to `hrflow-connectors` contributor's guide.**

This document focuses on getting any potential contributor familiarized
with the development processes, but **other kinds of contributions** are also
appreciated.

If you are new to using *git* or have never collaborated in a project previously,
please have a look at [contribution-guide](http://contribution-guide.org). Other resources are also listed in the excellent guide created by FreeCodeCamp [^contrib1]

Please notice, all users and contributors are expected to be **open,
considerate, reasonable, and respectful**. When in doubt, *Python Software
Foundation's Code of Conduct* is a good reference in terms of behavior
guidelines.


## Issue Reports

If you experience bugs or general issues with `hrflow-connectors`, please have a look
on the `issue tracker`. If you don't see anything useful there, please feel
free to fire an issue report.

:::info
ℹ️ Please don't forget to include the closed issues in your search. Sometimes a solution was already reported, and the problem is considered **solved**.
:::

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
*This page is based on the automatically generated file : PyScaffold CONTRIBUTING.rst.*
