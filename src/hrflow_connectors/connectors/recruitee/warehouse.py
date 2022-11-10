import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.recruitee.schemas import (
    RecruiteeJob,
    RecruiteeProfile,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class Sort(str, Enum):
    BY_DATE = "by_date"
    BY_LAST_MESSAGE = "by_last_message"


class Endpoint(str, Enum):
    STAGING_ENDPOINT = "STAGING ENDPOINT"
    PRODUCTION_ENDPOINT = "PRODUCTION ENDPOINT"


ENDPOINT_MAPPING = {
    Endpoint.STAGING_ENDPOINT: "https://api.s.recruitee.com/c",
    Endpoint.PRODUCTION_ENDPOINT: "https://api.recruitee.com/c",
}


class ReadProfilesParameters(ParametersModel):
    company_id: str = Field(
        ...,
        description="Company ID. A company subdomain can also be used.",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    recruitee_endpoint: Endpoint = Field(
        ...,
        description="Specifies which endpoint to be used, satging or production.",
        field_type=FieldType.Other,
    )
    limit: int = Field(
        None,
        description="Specifies the number of candidates to retrieve",
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        None,
        description=(
            "Skip number of candidates from the begining, used for ‘load more’, offset"
            " for next page should be current offset + limit"
        ),
        field_type=FieldType.QueryParam,
    )
    created_after: str = Field(
        None,
        description="Show only candidates created after given date",
        field_type=FieldType.QueryParam,
    )
    disqualified: bool = Field(
        None,
        description=(
            """Show only disqualified candidates who are disqualified in at least"""
            """ one job (should be string ‘true’ or ‘1’)."""
        ),
        field_type=FieldType.QueryParam,
    )
    qualified: bool = Field(
        None,
        description=(
            "Show only disqualified candidates who are qualified in at least one job"
            " (should be string ‘true’ or ‘1’)."
        ),
        field_type=FieldType.QueryParam,
    )
    ids: str = Field(
        None,
        description="List of IDs separated by comma, example: 234221,4211412,535432",
        field_type=FieldType.QueryParam,
    )
    offer_id: str = Field(
        None,
        description="Filter by offer",
        field_type=FieldType.QueryParam,
    )
    query: str = Field(
        None,
        description="Search query for candidate’s name or offer",
        field_type=FieldType.QueryParam,
    )
    sort: Sort = Field(
        None,
        description="Sorting options: by_date, by_last_message",
        field_type=FieldType.QueryParam,
    )
    with_messages: bool = Field(
        None,
        description=(
            "Show only candidates with messages (should be string ‘true’ or ‘1’)"
        ),
        field_type=FieldType.QueryParam,
    )
    with_my_messages: bool = Field(
        None,
        description=(
            "Show only candidates with messages that current admin sent (should be"
            " string ‘true’ or ‘1’"
        ),
        field_type=FieldType.QueryParam,
    )


class WriteProfilesParameters(ParametersModel):
    company_id: str = Field(
        ...,
        description="Company ID. A company subdomain can also be used.",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    recruitee_endpoint: Endpoint = Field(
        ...,
        description="Specifies which endpoint to be used, satging or production.",
        field_type=FieldType.Other,
    )
    offer_ids: t.List[int] = Field(
        None,
        description=(
            "Offers to which the candidate will be assigned with default stage. You can"
            " also pass one ID as offer_id"
        ),
        field_type=FieldType.QueryParam,
    )


class View_mode(str, Enum):
    DEFAULT = "default"
    BRIEF = "brief"


class ReadJobsParameters(ParametersModel):
    company_id: str = Field(
        ...,
        description="Company ID. A company subdomain can also be used.",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    recruitee_endpoint: Endpoint = Field(
        ...,
        description="Specifies which endpoint to be used, satging or production.",
        field_type=FieldType.Other,
    )

    kind: str = Field(
        None,
        description=(
            "If no kind is given, returns all job offers, if kind is job then lists"
            " only jobs, if scope is talent_pool, lists only talent pools"
        ),
        field_type=FieldType.QueryParam,
    )
    scope: str = Field(
        None,
        description=(
            "If no scope is given list all job offers. archived returns only archived"
            " job offers, active returns published, internal and closed job offers,"
            " not_archived returns all but archived jobs"
        ),
        field_type=FieldType.QueryParam,
    )
    view_mode: View_mode = Field(
        default="brief",
        description=(
            "default (default mode, includes most of offer details); brief (only"
            " offer’s id, title, status and kind)"
        ),
        field_type=FieldType.QueryParam,
    )


class WriteJobsParameters(ParametersModel):
    company_id: str = Field(
        ...,
        description="Company ID. A company subdomain can also be used.",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    recruitee_endpoint: Endpoint = Field(
        ...,
        description="Specifies which endpoint to be used, satging or production.",
        field_type=FieldType.Other,
    )


def read_candidates(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    RECRUITEE_ENDPOINT = ENDPOINT_MAPPING[parameters.recruitee_endpoint]
    params = parameters.dict()
    del params["api_token"]
    del params["company_id"]
    del params["recruitee_endpoint"]

    response = requests.get(
        "{}/{}/candidates".format(RECRUITEE_ENDPOINT, parameters.company_id),
        headers={
            "Authorization": "Bearer {}".format(parameters.api_token),
        },
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull candidates list from Recruitee params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
    candidates = response.json()["candidates"]
    for candidate in candidates:
        yield candidate


def write_candidates(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Adding {} profiles to Recruitee ".format(len(profiles)))
    RECRUITEE_ENDPOINT = ENDPOINT_MAPPING[parameters.recruitee_endpoint]
    failed_profiles = []
    for profile in profiles:
        payload = {"candidate": profile, "offers": parameters.offer_ids}

        response = requests.post(
            "{}/{}/candidates".format(RECRUITEE_ENDPOINT, parameters.company_id),
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(parameters.api_token),
            },
            json=payload,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add candidate to Recruitee status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    RECRUITEE_ENDPOINT = ENDPOINT_MAPPING[parameters.recruitee_endpoint]
    params = dict(
        kind=parameters.kind,
        scope=parameters.scope,
        view_mode=parameters.view_mode,
    )

    response = requests.get(
        "{}/{}/offers".format(RECRUITEE_ENDPOINT, parameters.company_id),
        headers={"Authorization": "Bearer {}".format(parameters.api_token)},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from Recruitee params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull jobs from Recruitee")
    jobs = response.json()["offers"]
    for job in jobs:
        full_job_response = requests.get(
            "{}/{}/offers/{}".format(
                RECRUITEE_ENDPOINT, parameters.company_id, job["id"]
            ),
            headers={"Authorization": "Bearer {}".format(parameters.api_token)},
        )
        if full_job_response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs details from Recruitee job_id={}"
                " status_code={} response={}".format(
                    job["id"],
                    full_job_response.status_code,
                    full_job_response.text,
                )
            )
            raise Exception("Failed to pull jobs from Recruitee")
        yield full_job_response.json()["offer"]


def write_jobs(
    adapter: LoggerAdapter,
    parameters: WriteJobsParameters,
    jobs: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Adding {} job offers to Recruitee ".format(len(jobs)))
    RECRUITEE_ENDPOINT = ENDPOINT_MAPPING[parameters.recruitee_endpoint]
    failed_jobs = []
    for job in jobs:
        payload = {"offer": job}
        response = requests.post(
            "{}/{}/offers".format(RECRUITEE_ENDPOINT, parameters.company_id),
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(parameters.api_token),
            },
            json=payload,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add job offers to Recruitee status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_jobs.append(job)
    return failed_jobs


RecruiteeProfileWarehouse = Warehouse(
    name="Recruitee Profiles",
    data_schema=RecruiteeProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_candidates,
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_candidates,
    ),
)


RecruiteeJobWarehouse = Warehouse(
    name="Recruitee Jobs",
    data_schema=RecruiteeJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
    write=WarehouseWriteAction(
        parameters=WriteJobsParameters,
        function=write_jobs,
    ),
)
