from pydantic import BaseModel
from typing import Tuple

from .actions import PullJobs


class Spec(BaseModel):
    actions: Tuple[PullJobs]