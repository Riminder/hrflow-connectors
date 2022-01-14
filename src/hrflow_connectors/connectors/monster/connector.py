from hrflow import Hrflow
from typing import Optional, Dict, Any

from ...core.connector import Connector
from .actions import CatchProfileAction


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
