from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from .actions import PullJobsAction


class Ceridian(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow,
        board_key: str,
        subdomain: str,
        client_name_space: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:

        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            subdomain=subdomain,
            client_name_space=client_name_space,
            **kwargs
        )
        return action.execute()
