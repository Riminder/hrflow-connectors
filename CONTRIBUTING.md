# Contributing

**Welcome to [hrflow-connectors](https://github.com/Riminder/hrflow-connectors) contributor's guide.**

We **thank you for your interest** in contributing to our open source project [hrflow-connectors](https://github.com/Riminder/hrflow-connectors).

If you are a first time contributor we recommend reading the project [documentation](./DOCUMENTATION.md). Following the [`LocalJSON` tutorial](./DOCUMENTATION.md#connector-developpment-tutorial-localjson) should give you a hands-on introduction to the _HrFlow.ai Connectors' Framework_.

You can also refer to the [hrflow developers documentation](https://developers.hrflow.ai/reference/authentication) for matters related to other HrFlow.ai services.

_We welcome any contributions from the community big or small._

## Code of conduct
Please notice, all users and contributors are expected to be **open,
considerate, reasonable, and respectful**. When in doubt, *Python Software
Foundation's Code of Conduct* is a good reference in terms of behavior
guidelines.


## Environment setup
1. Fork the repository
2. Make sure you have `python` _3.10.5_
3. Install [poetry](https://python-poetry.org/docs/)
4. Install dependencies by running `poetry install`
5. **[Optional]** Install [git-lfs](https://git-lfs.github.com/) if necessary. _Run `sudo apt-get install git-lfs` for Ubuntu 18.04, Debian 10, and newer versions_
6. **[Optional]** Install developpment hooks `make init-hooks`. This installs `git-lfs` add `pre-commit` hooks defined in [`.pre-commit-config.yaml`](.pre-commit-config.yaml)

### Instructions for `pyenv` users
1. Install python version `3.10.5` for example `pyenv install 3.10.5`
2. Activate version `pyenv local 3.10.5`
3. Check that version is activated `python --version` should print `3.10.5`
4. Force `poetry` to use `pyenv` python version `poetry env use $(pyenv which python)`
5. Check that `poetry` uses correct python version `poetry run python --version` should print `3.10.5`
6. Then run `poetry install`
7. **[Optional]** If you work on code related to S3 backend store then install the **s3** extras by running `poetry install -E s3`
> **Mind** that if you activate this extra you will need to export the following environment variables before executing tests locally 
```bash
export S3_STORE_TEST_BUCKET="hrflow-connectors-cicd"
export S3_STORE_TEST_AWS_REGION="eu-west-1"
export S3_STORE_TEST_AWS_ACCESS_KEY_ID=***REACH OUT INTERNALLY***
export S3_STORE_TEST_AWS_SECRET_ACCESS_KEY=***REACH OUT INTERNALLY***

export S3_STORE_TEST_READ_ONLY_AWS_ACCESS_KEY_ID=***REACH OUT INTERNALLY***
export S3_STORE_TEST_READ_ONLY_AWS_SECRET_ACCESS_KEY=***REACH OUT INTERNALLY***

export S3_STORE_TEST_WRITE_ONLY_AWS_ACCESS_KEY_ID=***REACH OUT INTERNALLY***
export S3_STORE_TEST_WRITE_ONLY_AWS_SECRET_ACCESS_KEY=***REACH OUT INTERNALLY***
```



## Contribution checklist

For all kind of contributions make sure that :
1. Your work respects the folder and file naming conventions in the [Connector Developpment Tutorial](./DOCUMENTATION.md#folder-structure)
2. Make sure that [`manifest.json`](./manifest.json) was updated to reflect your work. See [here](./DOCUMENTATION.md#add-localjson-to-the-hrflowai-connectors-manifest) for instructions
3. Make sure that documentation was updated to reflect your work. See [here](./DOCUMENTATION.md#generate-documentation-for-localjson) for instructions
4. Enforce code style by running `make style`
5. Ensure proper linting by running `make flake8`
6. Make sure that **core** tests are ok. These do not involve any connector code so you should'nt need to provide any kind of secrets. Run `make pytest-core`. _If you introduced new functionality in **core** make sure to maintain a _100%_ coverage score_.
7. **[Optional]** [_Only if your are contributing a new connector_] Make sure that your own connector tests are ok. Run `poetry run pytest --no-cov --ignore tests/core --connector=${YOUR_CONNECTOR_NAME}`
8. If you reach this step then your PR is very welcome ! From your fork you can target our `master` branch


## Issue Reports

If you experience bugs or general issues with [hrflow-connectors](https://github.com/Riminder/hrflow-connectors), please have a look at the [issue tracker](https://github.com/Riminder/hrflow-connectors/issues) before firing a new issue report.

ℹ️ Please don't forget to include the closed issues in your search.

New issue reports should include information about your programming environment (e.g., operating system, Python version) and steps to reproduce the problem.
Please try to simplify the reproduction steps to a very minimal example that still illustrates the problem you are facing. By removing other factors, you help us identify the root cause of the issue.