from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from .actions import PullJobsAction


class XML(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow,
        board_key: str,
        xml_stream_url: str,
        job_list_xpath: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            xml_stream_url=xml_stream_url,
            job_list_xpath=job_list_xpath,
            **kwargs
        )
        return action.execute()