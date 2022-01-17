from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import OAuth2Session
from ...utils.hrflow import Profile
from .actions import PushProfileAction


class Bullhorn(Connector):
    @staticmethod
    def push_profile(auth: OAuth2Session,
                 hrflow_client: Hrflow,
                 subdomain: str,
                 profile: Profile,
                 **kwargs
            ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            subdomain=subdomain,
            profile=profile,
            **kwargs
        )
        return action.execute()
