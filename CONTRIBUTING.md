# Contributing

**Welcome to [hrflow-connectors](https://github.com/Riminder/hrflow-connectors) contributor's guide.**

We **thank you for your interest** in contributing to our open source project [hrflow-connectors](https://github.com/Riminder/hrflow-connectors).

If you are a first time contributor we recommend reading the project [documentation](./DOCUMENTATION.md). Following the [`LocalJSON` tutorial](./DOCUMENTATION.md#connector-developpment-tutorial-localjson) should give you a hands-on introduction to the _HrFlow.ai Connectors' Framework_.

You can also refer to the [hrflow developers documentation](https://developers.hrflow.ai/reference/authentication) for matters related to other HrFlow.ai services.

_We welcome any contributions from the community big or small._

## Code of conduct

Please notice, all users and contributors are expected to be **open,
considerate, reasonable, and respectful**. When in doubt, _Python Software
Foundation's Code of Conduct_ is a good reference in terms of behavior
guidelines.

## Branch Policy

The `master` branch is the stable version of the package where tests and other enforced policies (code style, etc...) should be always passing.

The `dev` branch can be used to merge various contributions and test them in conjunction while bypassing the sometimes time consuming process of conforming to `master` merging rules.

Depending on the version you would like to use you can `git checkout origin (master|dev)`.

**_Mind that your PR should ultimately target `master` and be based on `master`_**

## Versionning and Git Commit guidelines

Automatic versionning is enabled for the `master` branch. For each `push` [Python Semantic Release (PSR)](https://python-semantic-release.readthedocs.io/en/latest/index.html) is used in order to release a new version to Github.
[PSR](https://python-semantic-release.readthedocs.io/en/latest/index.html) uses commit based rules to determine if a new version should be published and how the version number should be bumped.

[PSR](https://python-semantic-release.readthedocs.io/en/latest/changelog_templates.html#) does also update the [CHANGELOG.md](./CHANGELOG.md) file following the custom template defined in [templates/CHANGELOG.md.j2](./templates/CHANGELOG.md.j2)

The current parsing rules are based on [Angular Commit Guidelines](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#commits). This is configured by `commit_parser` in `[tool.semantic_release]` section of [pyproject.toml](./pyproject.toml). See [here](https://python-semantic-release.readthedocs.io/en/latest/commit-parsing.html) for details about commit parsing for PSR.

The behavior can be further customized by changing any of `allowed_tags`, `minor_tags` or `patch_tags` in the `[tool.semantic_release.commit_parser_options]` section of [pyproject.toml](./pyproject.toml).

Below we give some example of what is expected to happen given different situations.

> You can also check the [Automatic Version bumping section](https://py-pkgs.org/07-releasing-versioning#automatic-version-bumping) of [Python Packages e-book](https://py-pkgs.org/welcome) for other examples.

Assume that the last release is `v4.5.22` having:
- _MAJOR_: 4
- _MINOR_: 5
- _PATCH_: 22

Then branch `feature/some-new-feature` is merged onto `master`.

We examine the following three scenarios with regards to the new commits brought to `master` after the merge.
### First case
- Commit 1
```
docs: update the documentation generation process

some changes were made to the generate_documentation helper
in core
```
- Commit 2
```
refactor: create a helper enum to hold possible value for trigger_type
```

The parsed tags for this commit history are `docs` and `refactor`.

None of them are part of `minor_tags` nor `patch_tags` and there is no mention of any **BREAKING CHANGE** in any of the commit bodies.

**Result:** _After merge PSR does not trigger any release. The last release remains `v4.5.22`_

### Second case
- Commit 1
```
docs: update the documentation generation process
some changes were made to the generate_documentation helper
in core
```
- Commit 2
```
fix: correct a bug in MyConnector profile Warehouse
```
- Commit 3
```
refactor: create a helper enum to hold possible value for trigger_type
```

The parsed tags for this commit history are `docs`, `fix` and  `refactor`.

There is no mention of any **BREAKING CHANGE** in any of the commit bodies but `fix` is within the currently configured `patch_tags` in `[tool.semantic_release.commit_parser_options]` section of [pyproject.toml](./pyproject.toml).

**Result:** _After merge PSR does increment the _**PATCH**_ tag making a **new release** `v4.5.`**`23`**_

### Third case
- Commit 1
```
docs: update the documentation generation process
some changes were made to the generate_documentation helper
in core
```
- Commit 2
```
feat: add a new MyNewConnector to connectors' list
```
- Commit 3
```
refactor: create a helper enum to hold possible value for trigger_type
```

The parsed tags for this commit history are `docs`, `feat`  `refactor`.

There is no mention of any **BREAKING CHANGE** in any of the commit bodies but `feat` is within the currently configured `minor_tags` in `[tool.semantic_release.commit_parser_options]` section of [pyproject.toml](./pyproject.toml).

**Result** => _After merge PSR does increment the _**MINOR**_ tag making a **new release** `v4.`**`6`**`.0`_

### Third case
- Commit 1
```
docs: update the documentation generation process
some changes were made to the generate_documentation helper
in core
```
- Commit 2
```
feat: remove previously deprecated helper
BREAKING CHANGE: Warehouse.my_removed_helper  is not longer available
```
- Commit 3
```
refactor: create a helper enum to hold possible value for trigger_type
```

The parsed tags for this commit history are `docs`, `feat`  `refactor`.

There is a mention of a **BREAKING CHANGE** in the second commit.

**Result:** _After merge PSR does increment the _**MAJOR**_ tag making a **new release** `v`**`5`**`.0.0`_

## Basic Environment setup

1. Fork the repository
2. Make sure you have [one of the supported `python` versions](./pyproject.toml) installed
3. Install [poetry](https://python-poetry.org/docs/)
4. Install dependencies by running `poetry install`
5. **[Optional]** Install [git-lfs](https://git-lfs.github.com/) if necessary. _Run `sudo apt-get install git-lfs` for Ubuntu 18.04, Debian 10, and newer versions_
6. **[Optional]** Install developpment hooks `make init-hooks`. This installs `git-lfs` add `pre-commit` hooks defined in [`.pre-commit-config.yaml`](.pre-commit-config.yaml)

### Activating the S3 Extra

If you are contributing work related to the [S3 backend store](./DOCUMENTATION.md#s3store) you need to update step 4 of the [Basic Environment setup](#basic-environment-setup) with:

1. `poetry install -E s3`
2. Properly configure the store as described [here](./DOCUMENTATION.md#backend-configuration)

### Runing `nox` sessions

`nox` sessions are used to test core components across supported versions of Python. The following sessions are defined:

- `manifest`: Generates the `manifest.json` file across supported versions. Fails if the `manifest.json` file is different for any given version
- `docs`: Genetates documentation files accross supported versions. Fails if any file is different for any given version
- `tests`: Run tests without **s3** extras
- `tests_s3`: Run tests with **s3** extras

In order to run the `nox` sessions you must:

1. Install locally a python interpreter matching each of the supported major versions
2. Run `poetry run nox -s manifest` to test the `poetry run nox -s manifest` suite

> By default both `tests` and `tests_s3` run _core tests only_. To run tests for connectors use one of the following commands:
>
> ```bash
> # Only connector's tests
> poetry run nox -s (tests|tests_s3) -- --no-cov --ignore tests/core --allconnectors
> # Only a specific connector's tests
> poetry run nox -s (tests|tests_s3) -- --no-cov --ignore tests/core > --connector=SomeConnector
> # Core + All Connectors + Coverage
> poetry run nox -s (tests|tests_s3) -- --allconnectors
> ```

> In order to run the `tests_s3` session you need to export the following environment variables
>
> ```bash
> export S3_STORE_TEST_BUCKET="==========FILL==ME=========="
> export S3_STORE_TEST_AWS_REGION="==========FILL==ME=========="
> # This pair of credentials is expected to have
> # s3:GetObject, s3:ListBucket and s3:PutObject
> export S3_STORE_TEST_AWS_ACCESS_KEY_ID="==========FILL==ME=========="
> export S3_STORE_TEST_AWS_SECRET_ACCESS_KEY="==========FILL==ME=========="
> # This pair of credentials is expected to have **READ ONLY** access
> # s3:GetObject, s3:ListBucket
> export S3_STORE_TEST_READ_ONLY_AWS_ACCESS_KEY_ID="==========FILL==ME=========="
> export S3_STORE_TEST_READ_ONLY_AWS_SECRET_ACCESS_KEY="==========FILL==ME=========="
> # This pair of credentials is expected to have **WRITE ONLY** access
> # s3:PutObject, s3:ListBucket
> export S3_STORE_TEST_WRITE_ONLY_AWS_ACCESS_KEY_ID="==========FILL==ME=========="
> export S3_STORE_TEST_WRITE_ONLY_AWS_SECRET_ACCESS_KEY="==========FILL==ME=========="
> ```

### Instructions for `pyenv` users

#### Instructions for the basic setup

_Assuming you would like to use python `3.10.5`. Update instructions accordingly_

1. `pyenv install 3.10.5`
2. Activate version `pyenv local 3.10.5`
3. Check that version is activated `python --version` should print `3.10.5`
4. Force `poetry` to use `pyenv` python version `poetry env use $(pyenv which python)`
5. Check that `poetry` uses correct python version `poetry run python --version` should print `3.10.5`
6. Then run `poetry install`
7. **[Optional]** Activate **s3** extras `poetry install -E s3`

#### Instruction for running `nox`

_Hypothetically assuming that the supported versions are `3.10` and `3.11`. Update instructions accordingly_

1. Pick versions matching `3.10` and `3.11`. Below we use `3.10.10` and `3.11.2`
2. Run `pyenv install 3.10.10` and `pyenv install 3.11.2`
3. Add `.python-version` file in the top directory at the same level of [noxfile.py](./noxfile.py) with following content

```txt
3.10.10
3.11.2
```

4. If you run `pyenv versions` you should see that both versions are activated and set by `.python-version` file
5. Test that `nox` can properly use the python `3.10` and `3.11` by running `poetry run nox -s manifest -p 3.10 3.11`

## Contribution checklist

For all kind of contributions make sure that :

1. Your work respects the folder and file naming conventions in the [Connector Developpment Tutorial](./DOCUMENTATION.md#folder-structure)
2. Make sure that [`manifest.json`](./manifest.json) was updated to reflect your work. See [here](./DOCUMENTATION.md#add-localjson-to-the-hrflowai-connectors-manifest) for instructions
3. Make sure that documentation was updated to reflect your work. See [here](./DOCUMENTATION.md#generate-documentation-for-localjson) for instructions
4. Enforce code style by running `make style`
5. Ensure proper linting by running `make flake8`
6. Make sure that **core** tests are ok. These do not involve any connector code so you should'nt need to provide any kind of secrets. Run `make pytest-core`. _If you introduced new functionality in **core** make sure to maintain a \_100%_ coverage score\_.
7. **[Optional]** [_Only if your are contributing a new connector_] Make sure that your own connector tests are ok. Run `poetry run pytest --no-cov --ignore tests/core --connector=${YOUR_CONNECTOR_NAME}`
8. If you reach this step then your PR is very welcome ! From your fork you can target our `master` branch


## Required and Optional Flows for New Connectors

When contributing a new connector to `hrflow-connectors`, ensure you implement the following flows based on the connector type. These flows are critical for maintaining functional consistency across connectors.

### ATS/CRM/HCM Connectors

**Must Have (minimum 9 flows):**
1. Push profile to Connector:
   - `create_profiles_in_connector`
   - `update_profiles_in_connector`
   - `archive_profiles_in_connector`
2. Pull jobs from Connector:
   - `create_jobs_in_hrflow`
   - `update_jobs_in_hrflow`
   - `archive_jobs_in_hrflow`
3. Pull profiles from Connector:
   - `create_profiles_in_hrflow`
   - `update_profiles_in_hrflow`
   - `archive_profiles_in_hrflow`

**Nice to Have:**
1. Pull applications from Connector:
   - `create_applications_in_connector`
   - `update_applications_in_connector`

### Job Board Connectors

**Must Have (minimum 3 flows):**
1. Pull jobs from Connector:
   - `create_jobs_in_hrflow`
   - `update_jobs_in_hrflow`
   - `archive_jobs_in_hrflow`

**Nice to Have:**
1. Push profile to Connector:
   - `create_profiles_in_connector`
   - `update_profiles_in_connector`
   - `archive_profiles_in_connector`
2. Pull profiles from Connector:
   - `create_profiles_in_hrflow`
   - `update_profiles_in_hrflow`
   - `archive_profiles_in_hrflow`
3. Pull applications from Connector:
   - `create_applications_in_connector`
   - `update_applications_in_connector`

## Issue Reports

If you experience bugs or general issues with [hrflow-connectors](https://github.com/Riminder/hrflow-connectors), please have a look at the [issue tracker](https://github.com/Riminder/hrflow-connectors/issues) before firing a new issue report.

ℹ️ Please don't forget to include the closed issues in your search.

New issue reports should include information about your programming environment (e.g., operating system, Python version) and steps to reproduce the problem.
Please try to simplify the reproduction steps to a very minimal example that still illustrates the problem you are facing. By removing other factors, you help us identify the root cause of the issue.
