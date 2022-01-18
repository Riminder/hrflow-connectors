from hrflow import Hrflow
from typing import Optional, Dict, Any, Union

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class SapSuccessfactors(Connector):
    @staticmethod
    def pull_jobs(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        hrflow_client: Hrflow,
        board_key: str,
        api_server: str,
        top: int,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs from SAPSuccessFactors via their Job Requisition API. It adds all these jobs to a Hrflow.ai Board.

        `SAP(SuccessFactors)` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]): Auth instance to identify and communicate with the platform
            board_key (str): Board key where the jobs to be added will be stored
            api_server (str): the API server for your company from the list of API servers for SAP SuccessFactors data centers
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            hydrate_with_parsing (bool, optional): Enrich the job with parsing. Default value `False`
            archive_deleted_jobs_from_stream (bool, optional): Archive Board jobs when they are no longer in the incoming job stream. Default value `True`
            top (int, optional): show only the first n items, value by default = `20`. Default value `20`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PullJobsAction(
            auth=auth,
            hrflow_client=hrflow_client,
            board_key=board_key,
            api_server=api_server,
            top=top,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        hrflow_client: Hrflow,
        profile: Profile,
        api_server: str,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a `Profile` from a HrFlow Source to a SAP SuccessFactors Jobs pool.

        `Hrflow.ai` -> `SAP SuccessFactors`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            api_server (str): the API server for your company from the list of API servers for SAP SuccessFactors data centers
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            profile_already_exists (Any, optional): profile already exists !. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            api_server=api_server,
            **kwargs
        )
        return action.execute()
