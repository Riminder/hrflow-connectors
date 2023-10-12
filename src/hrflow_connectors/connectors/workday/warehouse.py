from pydantic import Field, validator
import typing as t

from hrflow_connectors.core import ParametersModel, FieldType
from hrflow_connectors.connectors.workday.utils.errors import (
    WorkdayNumberOutOfBoundsError,
)
from hrflow_connectors.connectors.workday.schemas import (
    WorkdayCandidate,
    WorkdayDescriptorId,
    WorkdayEducation,
    WorkdayExperience,
    WorkdayId,
    WorkdayLanguage,
    WorkdayResumeAttachments,
    WorkdaySkill,
)


class WorkdayReadJobsParameters(ParametersModel):
    category: t.Optional[t.List[str]] = Field(
        None,
        description=(
            "The job family group for the job posting, according to the job"
            " requisition."
        ),
        field_type=FieldType.QueryParam,
    )
    jobSite: t.Optional[t.List[str]] = Field(
        None,
        description="The job posting sites that the job is posted on.",
        field_type=FieldType.QueryParam,
    )
    limit: int = Field(
        20,
        description=(
            "The maximum number of objects in a single response. The default is 20. The"
            " maximum is 100."
        ),
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        0,
        description=(
            "The zero-based index of the first object in a response collection. The"
            " default is 0. Use offset with the limit parameter to control paging of a"
            " response collection. Example: If limit is 5 and offset is 9, the response"
            " returns a collection of 5 objects starting with the 10th object."
        ),
        field_type=FieldType.QueryParam,
    )

    @validator("limit")
    @classmethod
    def _valid_limit(self, value: int) -> None:
        if value < 1 or value > 100:
            raise WorkdayNumberOutOfBoundsError("limit", value, 1, 100)
        return value

    @validator("offset")
    @classmethod
    def _valid_offset(self, value: int) -> None:
        if value < 0:
            raise WorkdayNumberOutOfBoundsError("offset", value, 0)
        return value


class WorkdayWriteProfileParameters(ParametersModel):
    candidateTags: t.Optional[t.List[WorkdayDescriptorId]] = Field(
        None,
        description="The candidate tags associated with the candidate.",
        field_type=FieldType.Other,
    )
    candidatePools: t.Optional[t.List[WorkdayDescriptorId]] = Field(
        None,
        description="The active, static pools for the candidate.",
        field_type=FieldType.Other,
    )
    candidate: WorkdayCandidate = Field(
        ..., description="The candidate profile associated with this ~Prospect~."
    )
    contactConsent: t.Optional[bool] = Field(
        None, description="If true, the candidate agrees to be contacted."
    )
    status: WorkdayId = Field(
        ..., description="Returns the ~Prospect~ Status for this ~Prospect~."
    )
    type: WorkdayId = Field(..., description="The type for the ~Prospect~.")
    source: WorkdayId = Field(
        ..., description="The source for the ~Prospect~ (linkedin, facebook, etc)."
    )
    level: WorkdayId = Field(
        ..., description="The targeted management level for the ~Prospect~."
    )
    referredBy: WorkdayId = Field(
        ..., description="The ~worker~ who referred the job application."
    )
    href: t.Optional[str] = Field(None, description="A link to the instance")
    educations: t.Optional[t.List[WorkdayEducation]] = Field(
        None, description="The educations associated with the candidate."
    )
    experiences: t.Optional[t.List[WorkdayExperience]] = Field(
        None, description="The experiences associated with the candidate."
    )
    skills: t.Optional[t.List[WorkdaySkill]] = Field(
        None, description="The skills associated with the candidate."
    )
    languages: t.Optional[t.List[WorkdayLanguage]] = Field(
        None, description="The languages associated with the candidate."
    )
    resume: t.Optional[WorkdayResumeAttachments] = Field(
        None, description="The resume file associated with the candidate."
    )
