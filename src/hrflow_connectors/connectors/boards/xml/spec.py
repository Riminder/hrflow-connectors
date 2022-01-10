from pydantic import BaseModel
from typing import Tuple

from .actions import XMLPullJobsAction


class Spec(BaseModel):
    actions: Tuple[XMLPullJobsAction]
