from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import XSmartTokenAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class SmartRecruiters(Connector):
    @staticmethod
    def pull_jobs(
        auth: XSmartTokenAuth, hrflow_client: Hrflow, board_key: str, **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            auth=auth, hrflow_client=hrflow_client, board_key=board_key, **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: XSmartTokenAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        job_id: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            job_id=job_id,
            **kwargs
        )
        return action.execute()