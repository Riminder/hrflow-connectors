DIR := $(PWD)

default: manifest

manifest:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__V1__, __CONNECTORS__V2__, hrflow_connectors_manifest as m1, hrflow_connectors_manifest_v2 as m2; m1(connectors=__CONNECTORS__V1__, directory_path="$(DIR)"); m2(connectors=__CONNECTORS__V2__, directory_path="$(DIR)", exclude_connectors=[c.model.name for c in __CONNECTORS__V1__])'

docs:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__V1__, __CONNECTORS__V2__, generate_docs as d1, generate_docs_v2 as d2; opensource_connectors, opensource_jobboards, premium_connectors, premium_jobboards = d1(connectors=__CONNECTORS__V1__, exclude_connectors=[c.model.name for c in __CONNECTORS__V2__]); d2(connectors=__CONNECTORS__V2__, only_connectors=[c.model.name for c in __CONNECTORS__V2__], opensource_connectors=opensource_connectors, opensource_jobboards=opensource_jobboards, premium_connectors=premium_connectors, premium_jobboards=premium_jobboards)'

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
