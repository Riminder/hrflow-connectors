from pydantic import BaseModel
from typing import Tuple

from .actions import SmartRecruitersPushProfileAction


class Spec(BaseModel):
    actions: Tuple[SmartRecruitersPushProfileAction]