from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import MonsterBodyAuth
from ...utils.hrflow import Job
from .actions import CatchProfileAction, PushJobAction


class Monster(Connector):
    @staticmethod
    def catch_profile(
        hrflow_client: Hrflow, source_key: str, request: Dict[str, Any], **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `CatchProfileAction` catches a Monster profile to `Hrflow.ai`.

        `Monster` -> `HrFlow.ai`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            source_key (str): Source key where the profiles to be added will be stored
            request (Dict[str, Any]): Body to format in HrFlow Profile
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = CatchProfileAction(
            hrflow_client=hrflow_client,
            source_key=source_key,
            request=request,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_job(
        auth: MonsterBodyAuth, hrflow_client: Hrflow, subdomain: str, job: Job, **kwargs
    ) -> Optional[Dict[str, Any]]:
        """
        `PushJobAction` pushes a Monster job to `Hrflow.ai`.

        `HrFlow.ai` -> `Monster`

        Args:
            hrflow_client (Hrflow): Hrflow client instance used to communicate with the Hrflow.ai API
            job (Job): Job to push
            subdomain (str): Subdomain monster just before `monster.com`. For example subdomain=`my_subdomain.my` in `https//my_subdomain.my.monster.com8443/bgwBroker`
            logics (List[str], optional): Function names to apply as filter before pushing the data. Default value `[]`
            global_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's global variables. Default value `None`
            local_scope (Optional[Dict[str, Any]], optional): A dictionary containing the current scope's local variables. Default value `None`
            format_function_name (Optional[str], optional): Function name to format job before pushing. Default value `None`

        Returns:
            Optional[Dict[str, Any]]: Workflow response or `None`
        """
        action = PushJobAction(
            auth=auth,
            hrflow_client=hrflow_client,
            subdomain=subdomain,
            job=job,
            **kwargs
        )
        return action.execute()
