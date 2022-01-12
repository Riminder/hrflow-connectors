from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from .actions import PullJobsAction


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