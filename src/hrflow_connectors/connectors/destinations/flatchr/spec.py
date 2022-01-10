from pydantic import BaseModel
from typing import Tuple

from .actions import FlatchrPushProfileAction, FlatchrEnrichProfileAction


class Spec(BaseModel):
    actions: Tuple[FlatchrPushProfileAction, FlatchrEnrichProfileAction]
