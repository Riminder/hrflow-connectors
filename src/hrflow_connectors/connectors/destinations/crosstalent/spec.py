from pydantic import BaseModel
from typing import Tuple

from .actions import ProfileDestinationAction


class Spec(BaseModel):
    actions: Tuple[ProfileDestinationAction]
