DIR := $(PWD)

default: manifest

manifest:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__, hrflow_connectors_manifest as m; m(connectors=__CONNECTORS__, directory_path="$(DIR)")'

docs:
	poetry run python -c 'from hrflow_connectors import __CONNECTORS__, generate_docs as m; m(connectors=__CONNECTORS__)'

hooks:
	poetry run pre-commit run --all-files

ipython:
	poetry run ipython
