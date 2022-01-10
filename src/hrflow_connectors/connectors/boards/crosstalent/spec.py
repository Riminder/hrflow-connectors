from pydantic import BaseModel
from typing import Tuple

from .actions import CrosstalentPullJobsAction


class Spec(BaseModel):
    actions: Tuple[CrosstalentPullJobsAction]
