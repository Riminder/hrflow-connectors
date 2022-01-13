from hrflow import Hrflow
from typing import Optional, Dict, Any, List

from ...core.connector import Connector
from ...core.auth import AuthorizationAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Recruitee(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, subdomain: str, **kwargs
    ) -> Optional[Dict[str, Any]]:

        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: AuthorizationAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        company_id: str,
        offer_id: Optional[List[int]],
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            company_id=company_id,
            offer_id=offer_id,
            **kwargs
        )
        return action.execute()
