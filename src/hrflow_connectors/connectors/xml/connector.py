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
        """
        `PullJobsAction` retrieves all jobs via an XML stream API. It adds all these jobs to a Hrflow.ai Board.

        `XML Stream` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            board_key (str): Board key where the jobs to be added will be stored
            xml_stream_url (str): URL to XML Stream
            job_list_xpath (str): XPath pointing to the job list in the XML stream
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            hydrate_with_parsing (bool, optional): Enrich the job with parsing. Default value `False`
            archive_deleted_jobs_from_stream (bool, optional): Archive Board jobs when they are no longer in the incoming job stream. Default value `True`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PullJobsAction(
            hrflow_client=hrflow_client,
            board_key=board_key,
            xml_stream_url=xml_stream_url,
            job_list_xpath=job_list_xpath,
            **kwargs
        )
        return action.execute()
