from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import OAuth2EmailPasswordBody

from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Breezyhr(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, auth: OAuth2EmailPasswordBody, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all your company available jobs in Breezyhr via their Breezy API. It adds all these jobs to a Hrflow.ai Board.

        `Breezyhr` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (OAuth2EmailPasswordBody): Auth instance to identify and communicate with the platform
            board_key (str): Board key where the jobs to be added will be stored
            company_name Optional[str]: Name of the company associated with the authenticated user, required if you haven't specified your company id. Default value `None`
            company_id Optional[str] : Id of the company associated with the authenticated user, Default value `None`
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
            hrflow_client=hrflow_client, board_key=board_key, auth=auth, **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: OAuth2EmailPasswordBody,
        hrflow_client: Hrflow,
        profile: Profile,
        position_id: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a Hrflow.ai profile to `Breezyhr` via their Salesforce API.

        `Hrflow.ai` -> `Breezyhr`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (OAuth2EmailPasswordBody): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            position_id str: Id of the position to create a new candidate for, required.
            company_name Optional[str]: Name of the company associated with the authenticated user, required if you haven't specified your company id. Default value `None`
            company_id Optional[str] : Id of the company associated with the authenticated user, Default value `None`
            origin Optional[str]: Indicates if the candidate is `sourced` or `applied`, Default value `sourced`
            cover_letter Optional[str]: Candidate's cover letter, default value `None`
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
            position_id=position_id,
            **kwargs
        )
        return action.execute()
