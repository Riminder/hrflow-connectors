from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Crosstalent(Connector):
    @staticmethod
    def pull_jobs(
        auth: OAuth2PasswordCredentialsBody,
        hrflow_client: Hrflow,
        board_key: str,
        subdomain: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs via their Salesforce API. It adds all these jobs to a Hrflow.ai Board.

        `Crosstalent` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (OAuth2PasswordCredentialsBody): Auth instance to identify and communicate with the platform
            board_key (str): Board key where the jobs to be added will be stored
            subdomain (str): Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http//my_subdomain.my.salesforce.com/ABC`
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
            auth=auth,
            hrflow_client=hrflow_client,
            board_key=board_key,
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: OAuth2PasswordCredentialsBody,
        hrflow_client: Hrflow,
        profile: Profile,
        subdomain: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a Hrflow.ai profile to `Crosstalent` via their Salesforce API.

        `Hrflow.ai` -> `Crosstalent`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (OAuth2PasswordCredentialsBody): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            subdomain (str): Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http//my_subdomain.my.salesforce.com/ABC`
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
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()
