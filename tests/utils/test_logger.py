import logging
from hrflow_connectors.utils.logger import LOGGER_NAME, get_logger, get_logger_with_basic_config


def test_get_logger_with_basic_config():
    logger = get_logger_with_basic_config()
    assert len(logger.handlers) >= 1
    handler_class_list = [handler.__class__ for handler in logger.handlers]
    assert logging.StreamHandler in handler_class_list