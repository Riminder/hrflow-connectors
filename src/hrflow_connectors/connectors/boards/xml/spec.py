from pydantic import BaseModel
from typing import Tuple

from .actions import XMLBoardAction


class Spec(BaseModel):
    actions: Tuple[XMLBoardAction]
