from logging import LoggerAdapter
from pydantic import Field, validator
import requests
import typing as t

from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)
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

    class Config:
        validate_assignment = True


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
        ...,
        description="The candidate profile associated with this ~Prospect~.",
        field_type=FieldType.Other,
    )
    contactConsent: t.Optional[bool] = Field(
        None,
        description="If true, the candidate agrees to be contacted.",
        field_type=FieldType.Other,
    )
    status: WorkdayId = Field(
        ...,
        description="Returns the ~Prospect~ Status for this ~Prospect~.",
        field_type=FieldType.Other,
    )
    type: WorkdayId = Field(
        ..., description="The type for the ~Prospect~.", field_type=FieldType.Other
    )
    source: WorkdayId = Field(
        ...,
        description="The source for the ~Prospect~ (linkedin, facebook, etc).",
        field_type=FieldType.Other,
    )
    level: WorkdayId = Field(
        ...,
        description="The targeted management level for the ~Prospect~.",
        field_type=FieldType.Other,
    )
    referredBy: WorkdayId = Field(
        ...,
        description="The ~worker~ who referred the job application.",
        field_type=FieldType.Other,
    )
    href: t.Optional[str] = Field(
        None, description="A link to the instance", field_type=FieldType.Other
    )
    educations: t.Optional[t.List[WorkdayEducation]] = Field(
        None,
        description="The educations associated with the candidate.",
        field_type=FieldType.Other,
    )
    experiences: t.Optional[t.List[WorkdayExperience]] = Field(
        None,
        description="The experiences associated with the candidate.",
        field_type=FieldType.Other,
    )
    skills: t.Optional[t.List[WorkdaySkill]] = Field(
        None,
        description="The skills associated with the candidate.",
        field_type=FieldType.Other,
    )
    languages: t.Optional[t.List[WorkdayLanguage]] = Field(
        None,
        description="The languages associated with the candidate.",
        field_type=FieldType.Other,
    )
    resume: t.Optional[WorkdayResumeAttachments] = Field(
        None,
        description="The resume file associated with the candidate.",
        field_type=FieldType.Other,
    )


def _workday_read_jobs(
    adapter: LoggerAdapter,
    parameters: WorkdayReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token = None  # TODO
    headers = {"accept": "application/json", "authorization": f"Bearer {token}"}
    params = parameters.model_dump(exclude_none=True)
    url = ""  # TODO

    while True:
        # TODO refresh token if needed
        response = requests.get(url, headers=headers, params=params)

        if response.status_code != requests.codes.ok:
            pass  # TODO handle error codes
        else:
            response_json = response.json()
            if response_json["total"] == 0:
                break
            data = response_json["data"]
            if isinstance(data, list):
                for job in data:
                    yield job
                if len(data) < params["limit"]:
                    break
            else:
                yield data

        params["offset"] += params["limit"]


def _workday_write_profile(
    adapter: LoggerAdapter,
    parameters: WorkdayWriteProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    pass


WorkdayProfilesWarehouse = Warehouse(
    name="Workday Profiles",
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        function=_workday_write_profile,
        parameters=WorkdayWriteProfileParameters,
        endpoints=[
            ActionEndpoints(
                name="Post Prospects",
                description=(
                    "Creates a single ~prospect~ instance with the specified data. In"
                    " the request body, specify at least the required field:"
                    " candidate.name.country.id. The Recruiting Name Components"
                    " configuration might require additional fields for candidate.name."
                    " To determine additional required fields, see the Recruiting Name"
                    " Components tab on the Maintain Name Components by ~Country~ task."
                ),
                url="",  # TODO
            )
        ],
    ),
)


WorkdayJobsWarehouse = Warehouse(
    name="Workday Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        function=_workday_read_jobs,
        parameters=WorkdayReadJobsParameters,
        endpoints=[
            ActionEndpoints(
                name="Get Job Postings",
                description=(
                    "Retrieves all job postings. You can filter by categories and job"
                    " sites."
                ),
                url="",  # TODO
            )
        ],
    ),
)
