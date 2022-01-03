from pydantic import BaseModel
from typing import Tuple

from .actions import IndeedFeed


class Spec(BaseModel):
    actions: Tuple[IndeedFeed]
