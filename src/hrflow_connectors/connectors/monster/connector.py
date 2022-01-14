from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from ...core.auth import MonsterBodyAuth
from ...utils.hrflow import Job
from .actions import CatchProfileAction, PushJobAction


class Monster(Connector):
    @staticmethod
    def catch_profile(hrflow_client: Hrflow, source_key: str, request: Dict[str, Any], **kwargs) -> Optional[Dict[str, Any]]:
        action = CatchProfileAction(
            hrflow_client=hrflow_client,
            source_key=source_key,
            request=request,
            **kwargs
        )
        return action.execute()

    @staticmethod
    def push_job(auth: MonsterBodyAuth,
                 hrflow_client: Hrflow,
                 subdomain: str,
                 job: Job,
                 **kwargs
            ) -> Optional[Dict[str, Any]]:
        action = PushJobAction(
            auth=auth,
            hrflow_client=hrflow_client,
            subdomain=subdomain,
            job=job,
            **kwargs
        )
        return action.execute()
