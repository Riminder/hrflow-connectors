from pydantic import BaseModel
from typing import Tuple

from .actions import CraigslistFeed


class Spec(BaseModel):
    actions: Tuple[CraigslistFeed]
