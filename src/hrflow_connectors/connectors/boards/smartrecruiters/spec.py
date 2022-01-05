from pydantic import BaseModel
from typing import Tuple

from .actions import GetAllJobs


class Spec(BaseModel):
    actions: Tuple[GetAllJobs]
