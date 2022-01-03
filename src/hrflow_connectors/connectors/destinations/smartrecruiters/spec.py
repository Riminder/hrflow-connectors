from pydantic import BaseModel
from typing import Tuple

from .actions import SmartProfile


class Spec(BaseModel):
    actions: Tuple[SmartProfile]