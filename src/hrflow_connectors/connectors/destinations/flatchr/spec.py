from pydantic import BaseModel
from typing import Tuple

from .actions import PushProfile, EnrichProfile


class Spec(BaseModel):
    actions: Tuple[PushProfile, EnrichProfile]
