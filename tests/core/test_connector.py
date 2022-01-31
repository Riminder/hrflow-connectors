from hrflow_connectors.core.connector import Connector


def test_Connector_pull_jobs(hrflow_client):
    try:
        Connector.pull_jobs(hrflow_client)
        assert False
    except NotImplementedError:
        assert True


def test_Connector_pull_profiles(hrflow_client):
    try:
        Connector.pull_profiles(hrflow_client)
        assert False
    except NotImplementedError:
        assert True


def test_Connector_push_job(hrflow_client):
    try:
        Connector.push_job(hrflow_client)
        assert False
    except NotImplementedError:
        assert True


def test_Connector_push_profile(hrflow_client):
    try:
        Connector.push_profile(hrflow_client)
        assert False
    except NotImplementedError:
        assert True