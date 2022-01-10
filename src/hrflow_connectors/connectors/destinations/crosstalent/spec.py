from pydantic import BaseModel
from typing import Tuple

from .actions import CrosstalentPushProfileAction


class Spec(BaseModel):
    actions: Tuple[CrosstalentPushProfileAction]
