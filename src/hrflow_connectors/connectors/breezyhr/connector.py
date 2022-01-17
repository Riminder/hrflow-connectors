from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import OAuth2EmailPasswordBody

from ...utils.hrflow import Profile
from .actions import PullJobsAction

class BreezyHr(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, auth: OAuth2EmailPasswordBody, **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            hrflow_client=hrflow_client, board_key=board_key, auth=auth, **kwargs
        )
        return action.execute()
