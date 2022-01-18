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
        """
        `PullJobsAction` gets all available jobs listed on Ceridian Dayforce specific endpoints. It adds all these jobs to a Hrflow.ai Board.

        `Ceridian Dayforce` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            board_key (str): Board key where the jobs to be added will be stored
            subdomain (str): subdomain just before `dayforcehcm.com`
            client_name_space (str): Uniquely identifies the client's Dayforce instance for example. Is needed to login
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            hydrate_with_parsing (bool, optional): Enrich the job with parsing. Default value `False`
            archive_deleted_jobs_from_stream (bool, optional): Archive Board jobs when they are no longer in the incoming job stream. Default value `True`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """

        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            subdomain=subdomain,
            client_name_space=client_name_space,
            **kwargs
        )
        return action.execute()
