DIR := $(PWD)/src/hrflow_connectors

default: manifest

manifest:
	PYTHONPATH=./src/ poetry run python -c 'from hrflow_connectors import __CONNECTORS__, hrflow_connectors_manifest as m; m(version="test", connectors=__CONNECTORS__, directory_path="$(DIR)")'

ipython:
	PYTHONPATH=./src/ poetry run ipython