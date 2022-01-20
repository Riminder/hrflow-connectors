import logging
import sys
from typing import Union

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


def get_logger_with_basic_config(
    level: Union[int, str] = logging.DEBUG
) -> logging.Logger:
    """
    Get logger with basic configuration :
    * `StreamHandler` on `stdout`
    * Level = `DEBUG`
    * Formatter = `[%(levelname)s][%(asctime)s][%(module)s:%(funcName)s:%(lineno)d] %(message)s`

    Args:
        level (Union[int, str], optional): Log level. Defaults to `logging.DEBUG`.

    Returns:
        logging.Logger: logger with basic configuration
    """
    logger = get_logger()
    logger.setLevel(level)

    stdout_handler = logging.StreamHandler(sys.stdout)
    stdout_handler.setLevel(level)

    formatter = logging.Formatter(
        "[%(levelname)s][%(asctime)s][%(module)s:%(funcName)s:%(lineno)d] %(message)s"
    )
    stdout_handler.setFormatter(formatter)

    logger.addHandler(stdout_handler)

    return logger
