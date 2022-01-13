from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from .actions import PullJobsAction


class Workable(Connector):
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
