import os
from dotenv import load_dotenv

from .logger import get_logger

logger = get_logger()


class ConfigError(Exception):
    """
    Config Error
    """

    pass


class Config:
    """
    Config class

    Add the configuration line in `.env` to the root of the project.
    You must start the environment variable name with `HRFLOW_CONNECTORS`.
    For example: `HRFLOW_CONNECTORS_COMPANY_TOKEN = "abc"`.

    Instantiate `Config` and call the newly created attribute (without `HRFLOW_CONNECTORS_` prefix).
    If the environment variable is not found, then it will raise a `ConfigError`.

    >>> config = Config()
    >>> config.COMPANY_TOKEN
    'abc'
    >>> config.OTHER_TOKEN
    ConfigError: Unable to get the value of the `OTHER_TOKEN` field from the configuration. Check that the field `HRFLOW_CONNECTORS_OTHER_TOKEN` is initialized in `.env` at the root of the project.
    """

    def __init__(self):
        load_dotenv()
        for key, value in os.environ.items():
            if key.startswith("HRFLOW_CONNECTORS_"):
                prefix_last_char_position = len("HRFLOW_CONNECTORS_")
                # truncate var name to remove `HRFLOW_CONNECTORS_`
                attribute_name = key[prefix_last_char_position:]
                self.__setattr__(attribute_name, value)

    def __getattr__(self, name: str):
        """
        Default behavior if the attribute `name` is not found.

        Python call `__getattribute__` and if the attribute is not found
        Then Python call `__getattr__`.

        Args:
            name (str): Attribute name

        Raises:
            ConfigError: If the attribute with the `name` is not found
        """
        error_message = (
            f"Unable to get the value of the `{name}` field from the configuration. "
        )
        error_message += f"Check that the `HRFLOW_CONNECTORS_{name}` field is initialized in `.env` at the root of the project."
        logger.error(error_message)
        raise ConfigError(error_message)