from hrflow import Hrflow
from typing import Optional, Dict, Any, Union
from ...core.auth import AuthorizationAuth, OAuth2PasswordCredentialsBody
from ...core.connector import Connector
from .actions import PullJobsAction, PushProfileAction
from ...utils.schemas import HrflowProfile


class Workable(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str,auth: Union[AuthorizationAuth, OAuth2PasswordCredentialsBody], subdomain: str, **kwargs,
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs listed on Workable public endpoints. It adds all these jobs to a Hrflow.ai Board.

        `Workable` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (Union[AuthorizationAuth, OAuth2PasswordCredentialsBody]): Auth instance to identify and communicate with the platform
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
            hrflow_client=hrflow_client,
            board_key=board_key,
            auth=auth,
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: Union[AuthorizationAuth, OAuth2PasswordCredentialsBody], hrflow_client: Hrflow, profile: HrflowProfile, subdomain, shortcode, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a Hrflow.ai profile to `Workable` via their Workable API.
        `Hrflow.ai` -> `Workable`
        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (AuthorizationAuth): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            sourced (Optional[bool]): True if added by a recruiter without applying. Default value `False`
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth, hrflow_client=hrflow_client, profile=profile, subdomain=subdomain, shortcode=shortcode, **kwargs
        )
        return action.execute()
