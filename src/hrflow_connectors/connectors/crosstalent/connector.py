from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Crosstalent(Connector):
    @staticmethod
    def pull_jobs(
        auth: OAuth2PasswordCredentialsBody,
        hrflow_client: Hrflow,
        board_key: str,
        subdomain: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            auth=auth,
            hrflow_client=hrflow_client,
            board_key=board_key,
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: OAuth2PasswordCredentialsBody,
        hrflow_client: Hrflow,
        profile: Profile,
        subdomain: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()