from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import XTaleezAuth

from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Taleez(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, auth: XTaleezAuth, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs listed on a Taleez endpoint. It adds all these jobs to a Hrflow.ai Board.

        `Taleez` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (XTaleezAuth): Auth instance to identify and communicate with the platform
            board_key (str): Board key where the jobs to be added will be stored
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            hydrate_with_parsing (bool, optional): Enrich the job with parsing. Default value `False`
            archive_deleted_jobs_from_stream (bool, optional): Archive Board jobs when they are no longer in the incoming job stream. Default value `True`
            page (int, optional): Page number. Start at '0'. Default value `0`
            page_size (int, optional): Page size. Max size of the list returned. Max value  100. Default value `100`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PullJobsAction(
            hrflow_client=hrflow_client, board_key=board_key, auth=auth, **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: XTaleezAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        recruiter_id: int,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a `Profile` from a HrFlow Source to a Taleez Jobs pool.

        `Hrflow.ai` -> `Taleez`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            profile (Profile): Profile to push
            recruiter_id (int): ID of the person recruiting the candidate, mandatory
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            job_id (int, optional): ID of the job to add a candidate to. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            recruiter_id=recruiter_id,
            **kwargs
        )
        return action.execute()
