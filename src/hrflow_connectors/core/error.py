import requests

from ..utils.logger import get_logger
from typing import Dict, Any

logger = get_logger()


class ConnectorError(Exception):
    """
    Connector Error
    """

    pass


class AuthError(ConnectorError):
    """
    Auth Error
    """

    pass


class ActionError(ConnectorError):
    """
    Action Error
    """

    def __init__(self, response: requests.Response, title: str, **kwargs):
        """
        Action Error

        Args:
            response (requests.Response): server response
            title (str): title shown as the first error line

        >>> raise ActionError(response, "Failed to do", job_id=15, job_ref="abc")
        ... ActionError: Failed to do
        ... job_id: `15`
        ... job_ref: `abc`
        ... Status code: `400`
        ... Server response: `{message="error"}`
        """
        message = f"{title}\n"
        for field_name, field_value in kwargs.items():
            message += f"{field_name}: `{field_value}`\n"
        message += f"Status code: `{response.status_code}`\n"
        message += f"Server response: `{response.content}`"
        logger.error(title)
        super().__init__(message)


class PullError(ActionError):
    def __init__(self, response: requests.Response, **kwargs):
        """
        Pull Error

        Args:
            response (requests.Response): server response

        >>> response = requests.get("http://test.test/")
        >>> raise PullError(response, job_id=15, job_ref="abc")
        ... PullError: Failed to pull
        ... job_id: `15`
        ... job_ref: `abc`
        ... Status code: `400`
        ... Server response: `{message="error"}`
        """
        title = "Failed to pull"
        super().__init__(response, title, **kwargs)


class PushError(ActionError):
    def __init__(self, response: requests.Response, **kwargs):
        """
        Push Error

        Args:
            response (requests.Response): server response

        >>> response = requests.post("http://test.test/")
        >>> raise PushError(response, job_id=15, job_ref="abc")
        ... PushError: Failed to push
        ... job_id: `15`
        ... job_ref: `abc`
        ... Status code: `400`
        ... Server response: `{message="error"}`
        """
        title = "Failed to push"
        super().__init__(response, title, **kwargs)


class HrflowError(ConnectorError):
    """
    Hrflow Error
    """

    def __init__(self, response: Dict[str, Any], title: str, **kwargs):
        """
        Hrflow Error

        Args:
            response (Dict[str, Any]): Hrflow request response
            title (str): title shown as the first error line

        >>> client = hrflow.Hrflow(api_secret="x_api_key", api_user="x_user_email")
        >>> response = client.profile.indexing.get(...)
        >>> raise PushError(response, "Get Profile failed !", profile_key="abc", source_key="efg")
        ... HrflowError: Get Profile failed !
        ... profile_key: `abc`
        ... source_key: `efg`
        ... Status code: `500`
        ... Hrflow message: `Internal error`
        """
        message = f"{title}\n"
        for field_name, field_value in kwargs.items():
            message += f"{field_name}: `{field_value}`\n"

        status_code = response["code"]
        message += f"Status code: `{status_code}`\n"

        hrflow_message = response["message"]
        message += f"Hrflow message: `{hrflow_message}`"

        logger.error(title)
        super().__init__(message)