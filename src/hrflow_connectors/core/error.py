import requests

from ..utils.logger import get_logger

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

        >>> raise PullError(response, job_id=15, job_ref="abc")
        ... PullError: Failed to pull
        ... job_id: `15`
        ... job_ref: `abc`
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

        >>> raise PushError(response, job_id=15, job_ref="abc")
        ... PushError: Failed to push
        ... job_id: `15`
        ... job_ref: `abc`
        ... Server response: `{message="error"}`
        """
        title = "Failed to push"
        super().__init__(response, title, **kwargs)
