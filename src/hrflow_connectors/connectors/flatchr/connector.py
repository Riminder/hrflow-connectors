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
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            vacancy=vacancy,
            company=company,
            **kwargs
        )
        return action.execute()