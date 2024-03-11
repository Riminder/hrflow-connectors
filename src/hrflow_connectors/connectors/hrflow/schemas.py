import typing as t

from hrflow.hrflow.schemas import (  # noqa: F401
    GeneralEntitySchema,
    HrFlowJob,
    HrFlowProfile,
)
from pydantic import BaseModel, Field


class ResumeToParse(BaseModel):
    raw: bytes
    content_type: str


class HrFlowProfileParsing(BaseModel):
    reference: t.Optional[str] = Field(
        ..., description="Custom identifier of the Profile."
    )
    created_at: str = Field(
        ..., description="type: datetime ISO8601, Creation date of the Profile."
    )
    resume: ResumeToParse
    tags: t.List[GeneralEntitySchema] = Field(
        ..., description="List of tags of the Profile."
    )
    metadatas: t.List[GeneralEntitySchema] = Field(
        ..., description="List of metadatas of the Profile."
    )
