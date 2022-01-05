from pydantic import BaseModel
from typing import Tuple

from .actions import CareerBuilderFeed


class Spec(BaseModel):
    actions: Tuple[CareerBuilderFeed]