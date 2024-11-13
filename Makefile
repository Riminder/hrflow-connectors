DIR := $(PWD)

default: manifest

manifest:
	poetry run python -c 'from hrflow_connectors.v2 import __CONNECTORS__, hrflow_connectors_manifest as m; m(connectors=__CONNECTORS__, directory_path="$(DIR)")'

docs:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__ as __CONNECTORS__V1, generate_docs as docs_v1; from hrflow_connectors.v2 import __CONNECTORS__ as __CONNECTORS__V2, hrflow_connectors_docs as docs_v2; docs_v1(connectors=__CONNECTORS__V1); docs_v2(connectors=__CONNECTORS__V2)'

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
	HRFLOW_CONNECTORS_STORE_ENABLED="1" HRFLOW_CONNECTORS_LOCALJSON_DIR="/tmp/" poetry run pytest --allconnectors-v1 --allconnectors-v2

ipython:
	poetry run ipython
