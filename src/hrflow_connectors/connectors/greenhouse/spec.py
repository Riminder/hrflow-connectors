from pydantic import BaseModel
from typing import Tuple

from .actions import PushProfile


class Spec(BaseModel):
    actions: Tuple[PushProfile]