DIR := $(PWD)

default: manifest

manifest:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__, hrflow_connectors_manifest as m; m(connectors=__CONNECTORS__, directory_path="$(DIR)")'

docs:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__, generate_docs as m; m(connectors=__CONNECTORS__)'

init-hooks:
	git lfs update --force
	poetry run pre-commit install -t pre-commit
	poetry run pre-commit install -t pre-push

hooks:
	poetry run pre-commit run --all-files

style:
	poetry run isort .
	poetry run black --config=./pyproject.toml .

flake8:
	poetry run flake8 --config=./.flake8

pytest-core:
	HRFLOW_CONNECTORS_STORE_ENABLED="1" HRFLOW_CONNECTORS_LOCALJSON_DIR="/tmp/" poetry run pytest

pytest:
	HRFLOW_CONNECTORS_STORE_ENABLED="1" HRFLOW_CONNECTORS_LOCALJSON_DIR="/tmp/" poetry run pytest --allconnectors

ipython:
	poetry run ipython
