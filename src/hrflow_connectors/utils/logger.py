import logging
import sys

LOGGER_NAME = "hrflow_connectors"


def get_logger() -> logging.Logger:
    """
    Get logger with `NullHandler` by default
    Returns:
        logging.Logger: logger
    """
    logger = logging.getLogger(LOGGER_NAME)
    if not logger.hasHandlers():
        logger.addHandler(logging.NullHandler())
    return logger


def get_logger_with_basic_config() -> logging.Logger:
    logger = get_logger()
    logger.setLevel(logging.DEBUG)

    stdout_handler = logging.StreamHandler(sys.stdout)
    stdout_handler.setLevel(logging.DEBUG)

    formatter = logging.Formatter(
        "[%(levelname)s][%(asctime)s][%(module)s:%(funcName)s:%(lineno)d] %(message)s"
    )
    stdout_handler.setFormatter(formatter)

    logger.addHandler(stdout_handler)

    return logger