from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import XAPIKeyAuth

from ...utils.hrflow import Profile
from .actions import PullJobsAction


class Taleez(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, auth: XAPIKeyAuth, **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            hrflow_client=hrflow_client, board_key=board_key, auth=auth, **kwargs
        )
        return action.execute()
