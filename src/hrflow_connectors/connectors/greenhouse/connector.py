from hrflow import Hrflow
from typing import Optional, Dict, Any, Union, List

from ...core.connector import Connector
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth

from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Greenhouse(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, board_token: str, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs listed on Greenhouse board. It adds all these **jobs** to a Hrflow.ai Board.

        `Greenhouse` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            board_key (str): Board key where the jobs to be added will be stored
            board_token (str): Job Board URL token, which is usually the company `name` -for example `lyft`- when it has job listings on greenhouse, mandatory to access job boards on `greenhouse.io` `https//boards-api.greenhouse.io/v1/boards/{board_token}/jobs`, getting jobs doesn't require an API Key
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
            board_token=board_token,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth],
        job_id: List[int],
        on_behalf_of: str,
        hrflow_client: Hrflow,
        profile: Profile,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a `Profile` from a HrFlow Source to a Greenhouse Jobs pool.

        `Hrflow.ai` -> `Greenhouse`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            job_id (List[int]): List of jobs internal ids to which the candidate should be added
            on_behalf_of (str): The ID of the user sending the profile, or the person he is sending the profile on behalf of
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
            on_behalf_of=on_behalf_of,
            **kwargs
        )
        return action.execute()
