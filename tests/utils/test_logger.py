import logging
from hrflow_connectors.utils.logger import (
    LOGGER_NAME,
    get_logger,
    get_logger_with_basic_config,
)


def test_get_logger():
    # We need to disable propagation
    # so that we don't have the properties
    # like the list of handlers of the parent
    logger = get_logger()
    logger.propagate = False

    logger = get_logger()
    assert logger.hasHandlers()
    assert len(logger.handlers) == 1
    assert isinstance(logger.handlers[0], logging.NullHandler)


def test_get_logger_with_basic_config():
    # We need to disable propagation
    # so that we don't have the properties
    # like the list of handlers of the parent
    logger = get_logger()
    logger.propagate = False

    logger = get_logger_with_basic_config()
    assert len(logger.handlers) == 2

    handler_class_list = [handler.__class__ for handler in logger.handlers]
    assert logging.StreamHandler in handler_class_list
    assert logging.NullHandler in handler_class_list