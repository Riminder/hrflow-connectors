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
from hrflow_connectors.utils.config import Config


@pytest.fixture(scope="session")
def config() -> Config:
    """
    Get config from the `.env` file at the root of the project (to be defined)

    Returns:
        Config: Config instance
    """
    return Config()


@pytest.fixture(scope="session")
def hrflow_client(config) -> Callable:
    """
    Get a function to generate an instance of Hrflow Client

    Returns:
        Callable: Function to generate an instance of Hrflow Client

    Example :
    >>> def test_stuff(hrflow_client):
    >>>     client = hrflow_client(portal_name="MY_PORTAL_ON_HRFLOW")
    """

    def hrflow_client_func(portal_name="dev-demo"):
        if portal_name == "dev-demo":
            x_api_key = config.HRFLOW_DEVDEMO_XAPIKEY
            x_user_email = config.HRFLOW_DEVDEMO_XUSEREMAIL
        elif portal_name == "vulcain":
            x_api_key = config.HRFLOW_VULCAIN_XAPIKEY
            x_user_email = config.HRFLOW_VULCAIN_XUSEREMAIL
        else:
            raise RuntimeError(f"Hrflow Portal `{portal_name}` not found !")
        return Hrflow(api_secret=x_api_key, api_user=x_user_email)

    return hrflow_client_func


@pytest.fixture(scope="session")
def logger() -> logging.Logger:
    """
    Get `hrflow_connectors` Logger with basic config

    Returns:
        logging.Logger: `hrflow_connectors` Logger with basic config
    """
    return get_logger_with_basic_config()
