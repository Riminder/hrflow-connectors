from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import XSmartTokenAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class SmartRecruiters(Connector):
    @staticmethod
    def pull_jobs(
        auth: XSmartTokenAuth, hrflow_client: Hrflow, board_key: str, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` retrieves all jobs via the SmartRecruiter API. It adds all these jobs to a Hrflow.ai Board.

        `Smart Recruiters` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (XSmartTokenAuth): Auth instance to identify and communicate with the platform
            board_key (str): Board key where the jobs to be added will be stored
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            hydrate_with_parsing (bool, optional): Enrich the job with parsing. Default value `False`
            archive_deleted_jobs_from_stream (bool, optional): Archive Board jobs when they are no longer in the incoming job stream. Default value `True`
            query (str, optional): Full-text search query based on a job title; case insensitive; e.g. java developer. Default value `None`
            updated_after (str, optional): ISO8601-formatted time boundaries for the job update time. Default value `None`
            posting_status (str, optional): Posting status of a job. Available values  PUBLIC, INTERNAL, NOT_PUBLISHED, PRIVATE. Default value `None`
            job_status (str, optional): Status of a job. Available values  CREATED, SOURCING, FILLED, INTERVIEW, OFFER, CANCELLED, ON_HOLD. Default value `None`
            limit (int, optional): Number of elements to return per page. max value is 100. Default value  10. Default value `10`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PullJobsAction(
            auth=auth, hrflow_client=hrflow_client, board_key=board_key, **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: XSmartTokenAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        job_id: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a HrFlow.ai profile from a Hrflow.ai Source to `SmartRecruiters` via the SmartRecruiter API.

        `Hrflow.ai` -> `Smart Recruiters`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (XSmartTokenAuth): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            job_id (str): Id of a Job to which you want to assign a candidate when it’s created. A profile is sent to this URL `https//api.smartrecruiters.com/jobs/{job_id}/candidates`
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            job_id=job_id,
            **kwargs
        )
        return action.execute()
