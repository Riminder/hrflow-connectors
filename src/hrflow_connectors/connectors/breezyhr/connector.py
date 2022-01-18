from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import OAuth2EmailPasswordBody

from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class BreezyHr(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, auth: OAuth2EmailPasswordBody, **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            hrflow_client=hrflow_client, board_key=board_key, auth=auth, **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: OAuth2EmailPasswordBody,
        hrflow_client: Hrflow,
        profile: Profile,
        position_id: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            position_id=position_id,
            **kwargs
        )
        return action.execute()
