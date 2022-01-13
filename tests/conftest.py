"""
    Conftest for `hrflow_connectors`
    
    All the fixtures used throughout the tests are grouped here.
"""

import pytest
import os
import json
import logging
from typing import Dict, Any, Callable
from hrflow import Hrflow

from hrflow_connectors.utils.logger import get_logger_with_basic_config


@pytest.fixture(scope="session")
def credentials(pytestconfig) -> Dict[str, Any]:
    """
    Get credentials from a file in the root of the project `credentials.json` (to be defined)

    Returns:
        Dict[str, Any]: Credentials
    """
    with open(os.path.join(pytestconfig.rootpath, "credentials.json"), "r") as f:
        credentials = json.loads(f.read())
    return credentials


@pytest.fixture(scope="session")
def hrflow_client(credentials) -> Callable:
    """
    Get a function to generate an instance of Hrflow Client

    Returns:
        Callable: Function to generate an instance of Hrflow Client

    Example :
    >>> def test_stuff(hrflow_client):
    >>>     client = hrflow_client(portal_name="MY_PORTAL_ON_HRFLOW")
    """

    def hrflow_client_func(portal_name="dev-demo"):
        x_api_key = credentials["hrflow"][portal_name]["x-api-key"]
        x_user_email = credentials["hrflow"][portal_name]["x-user-email"]
        client = Hrflow(api_secret=x_api_key, api_user=x_user_email)
        return client

    return hrflow_client_func


@pytest.fixture(scope="session")
def logger() -> logging.Logger:
    """
    Get `hrflow_connectors` Logger with basic config

    Returns:
        logging.Logger: `hrflow_connectors` Logger with basic config
    """
    return get_logger_with_basic_config()
