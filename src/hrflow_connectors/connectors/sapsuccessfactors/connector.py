from hrflow import Hrflow
from typing import Optional, Dict, Any, Union

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class SapSuccessfactors(Connector):
    @staticmethod
    def pull_jobs(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        hrflow_client: Hrflow,
        board_key: str,
        api_server: str,
        top: int,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            auth=auth,
            hrflow_client=hrflow_client,
            board_key=board_key,
            api_server=api_server,
            top=top,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        hrflow_client: Hrflow,
        profile: Profile,
        api_server: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            api_server=api_server,
            **kwargs
        )
        return action.execute()