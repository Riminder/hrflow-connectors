from typing import Any, List, Optional

from pydantic import BaseModel


class Location(BaseModel):
    name: str


class Department(BaseModel):
    id: int
    name: str
    parent_id: Any
    child_ids: Optional[List[int]]


class Office(BaseModel):
    id: int
    name: str
    location: str
    parent_id: Optional[int]
    child_ids: Optional[List[int]]


class GreenhouseJobModel(BaseModel):
    id: int
    internal_job_id: int
    title: str
    updated_at: Optional[str]
    requisition_id: Optional[str]
    location: Location
    absolute_url: str
    metadata: Any
    content: str
    departments: List[Department]
    offices: List[Office]
