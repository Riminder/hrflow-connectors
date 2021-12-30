from pydantic import BaseModel
from typing import Tuple

from .actions import CraigslistJobs


class Spec(BaseModel):
    actions: Tuple[CraigslistJobs]
