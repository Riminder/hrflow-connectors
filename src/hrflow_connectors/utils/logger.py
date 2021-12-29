import logging

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