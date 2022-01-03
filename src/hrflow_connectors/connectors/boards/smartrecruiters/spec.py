from pydantic import BaseModel
from typing import Tuple

from .actions import SmartJobs


class Spec(BaseModel):
    actions: Tuple[SmartJobs]
