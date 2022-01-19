from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import AuthorizationAuth
from ...utils.hrflow import Profile
from .actions import PushProfileAction


class Flatchr(Connector):
    @staticmethod
    def push_profile(
        auth: AuthorizationAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        vacancy: str,
        company: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a Hrflow.ai profile to `Flatchr` via their API.

        `Hrflow.ai` -> `Flatchr`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (AuthorizationAuth): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            vacancy (str): The pool in which candidates will be placed. Findable in the URL
            company (str): The id of the company
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            vacancy=vacancy,
            company=company,
            **kwargs
        )
        return action.execute()
