from hrflow import Hrflow
from typing import Optional, Dict, Any, List

from ...core.connector import Connector
from ...core.auth import AuthorizationAuth
from ...utils.hrflow import Profile
from .actions import PullJobsAction, PushProfileAction


class Recruitee(Connector):
    @staticmethod
    def pull_jobs(
        hrflow_client: Hrflow, board_key: str, subdomain: str, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PullJobsAction` gets all available jobs listed on Recruitee company endpoints. It adds all these jobs to a Hrflow.ai Board.

        `Recruitee` -> `Hrflow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            board_key (str): Board key where the jobs to be added will be stored
            subdomain (str): the subdomain of your company's careers site.
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
            subdomain=subdomain,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_profile(
        auth: AuthorizationAuth,
        hrflow_client: Hrflow,
        profile: Profile,
        company_id: str,
        offer_id: Optional[List[int]],
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushProfileAction` pushes a `Profile` from a HrFlow Source to a Recruitee company endpoint and a optionally a Jobs pool.

        `Hrflow.ai` -> `Recruitee`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            auth (AuthorizationAuth): Auth instance to identify and communicate with the platform
            profile (Profile): Profile to push
            company_id (str): Company ID. A company subdomain can also be used.
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`
            offer_id (Optional[List[int]], optional): Offers to which the candidate will be assigned with default stage. You can also pass one ID as offer_id. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushProfileAction(
            auth=auth,
            hrflow_client=hrflow_client,
            profile=profile,
            company_id=company_id,
            offer_id=offer_id,
            **kwargs
        )
        return action.execute()
