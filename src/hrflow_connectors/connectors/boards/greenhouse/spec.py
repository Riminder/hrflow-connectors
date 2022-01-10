from pydantic import BaseModel
from typing import Tuple

from .actions import GreenhousePullJobsAction


class Spec(BaseModel):
    actions: Tuple[GreenhousePullJobsAction]