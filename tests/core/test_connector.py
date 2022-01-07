from hrflow_connectors.core.connector import Connector


def test_Connector_pull_jobs():
    try:
        Connector.pull_jobs()
        assert False
    except NotImplementedError:
        assert True