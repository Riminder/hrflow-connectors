from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth

from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Greenhouse(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, board_token: str, **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            board_token=board_token,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        job_id: List[int],
        on_behalf_of: str,
        hrflow_client: Hrflow,
        profile: Profile,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            job_id=job_id,
            on_behalf_of=on_behalf_of,
            **kwargs
        )
        return action.execute()