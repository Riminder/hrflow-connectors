from pydantic import BaseModel
from typing import Tuple

from .actions import SmartRecruitersPullJobsAction


class Spec(BaseModel):
    actions: Tuple[SmartRecruitersPullJobsAction]
