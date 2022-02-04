from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import OAuth2Session
from ...utils.schemas import HrflowProfile
from .actions import PushProfileAction


class Bullhorn(Connector):
    @staticmethod
    def push_profile(auth: OAuth2Session,
                 hrflow_client: Hrflow,
                 subdomain: str,
                 profile: HrflowProfile,
                 **kwargs
            ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a Hrflow.ai profile to `Bullhorn`.

        `Hrflow.ai` -> `Bullhorn`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (OAuth2Session): Auth instance to identify and communicate with the platform
            profile (HrflowProfile): Profile to push
            subdomain (str): Subdomain Bullhorn just before `bullhorn.com`. For example subdomain=`my_subdomain.my` in `http//my_subdomain.my.salesforce.com/ABC`
            logics (List[str], optional): Function names to apply as filter . Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            subdomain=subdomain,
            profile=profile,
            **kwargs
        )
        return action.execute()
