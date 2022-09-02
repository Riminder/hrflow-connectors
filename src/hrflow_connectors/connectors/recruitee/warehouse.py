import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.recruitee.schemas import (
    RecruiteeJob,
    RecruiteeProfile,
)
from hrflow_connectors.core import (
    DataType,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

RECRUITEE_ENDPOINT = "https://api.s.recruitee.com/c"


class Sort(str, Enum):
    BY_DATE = "by_date"
    BY_LAST_MESSAGE = "by_last_message"


class ReadProfilesParameters(BaseModel):
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used.", repr=False
    )
    API_Token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
    )
    limit: int = Field(
        None, description="Specifies the number of candidates to retrieve"
    )
    offset: int = Field(
        None,
        description=(
            "Skip number of candidates from the begining, used for ‘load more’, offset"
            " for next page should be current offset + limit"
        ),
    )
    created_after: str = Field(
        None, description="Show only candidates created after given date"
    )
    disqualified: bool = Field(
        None,
        description=(
            """Show only disqualified candidates who are disqualified in at least"""
            """ one job (should be string ‘true’ or ‘1’)."""
        ),
    )
    qualified: bool = Field(
        None,
        description=(
            "Show only disqualified candidates who are qualified in at least one job"
            " (should be string ‘true’ or ‘1’)."
        ),
    )
    ids: str = Field(
        None,
        description="List of IDs separated by comma, example: 234221,4211412,535432",
    )
    offer_id: str = Field(None, description="Filter by offer")
    query: str = Field(
        None,
        description="Search query for candidate’s name or offer",
    )
    sort: Sort = Field(None, description="Sorting options: by_date, by_last_message")
    with_messages: bool = Field(
        None,
        description=(
            "Show only candidates with messages (should be string ‘true’ or ‘1’)"
        ),
    )
    with_my_messages: bool = Field(
        None,
        description=(
            "Show only candidates with messages that current admin sent (should be"
            " string ‘true’ or ‘1’"
        ),
    )


class WriteProfilesParameters(BaseModel):
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used.", repr=False
    )
    API_Token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
    )
    offers: t.List[int] = Field(
        None,
        description=(
            "Offers to which the candidate will be assigned with default stage. You can"
            " also pass one ID as offer_id"
        ),
    )


class View_mode(str, Enum):
    DEFAULT = "default"
    BRIEF = "brief"


class ReadJobsParameters(BaseModel):
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used.", repr=False
    )
    API_Token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
    )
    kind: str = Field(
        None,
        description=(
            "If no kind is given, returns all job offers, if kind is job then lists"
            " only jobs, if scope is talent_pool, lists only talent pools"
        ),
    )
    scope: str = Field(
        None,
        description=(
            "If no scope is given list all job offers. archived returns only archived"
            " job offers, active returns published, internal and closed job offers,"
            " not_archived returns all but archived jobs"
        ),
    )
    view_mode: View_mode = Field(
        default="brief",
        description=(
            "default (default mode, includes most of offer details); brief (only"
            " offer’s id, title, status and kind)"
        ),
    )


class WriteJobsParameters(BaseModel):
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used.", repr=False
    )
    API_Token: str = Field(
        ...,
        description=(
            "Personal API Token allowing access to the Recruitee API from external"
            " services."
        ),
        repr=False,
    )


def read_candidates(parameters: ReadProfilesParameters) -> t.Iterable[t.Dict]:
    params = parameters.dict()
    del params["API_Token"]
    del params["company_id"]

    response = requests.get(
        "{}/{}/candidates".format(RECRUITEE_ENDPOINT, parameters.company_id),
        headers={
            "Authorization": "Bearer {}".format(parameters.API_Token),
        },
        params=params,
    )
    if response.status_code // 100 != 2:
        raise Exception(
            "Failed to pull candidates list from Recruitee."
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
    failed_profiles = []
    for profile in profiles:
        payload = {"candidate": profile, "offers": parameters.offers}

        response = requests.post(
            "{}/{}/candidates".format(RECRUITEE_ENDPOINT, parameters.company_id),
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(parameters.API_Token),
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
    adapter: LoggerAdapter, parameters: ReadJobsParameters
) -> t.Iterable[t.Dict]:
    params = dict(
        kind=parameters.kind,
        scope=parameters.scope,
        view_mode=parameters.view_mode,
    )

    response = requests.get(
        "{}/{}/offers".format(RECRUITEE_ENDPOINT, parameters.company_id),
        headers={"Authorization": "Bearer {}".format(parameters.API_Token)},
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
            headers={"Authorization": "Bearer {}".format(parameters.API_Token)},
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
    failed_jobs = []
    for job in jobs:
        payload = {"offer": job}
        response = requests.post(
            "{}/{}/offers".format(RECRUITEE_ENDPOINT, parameters.company_id),
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(parameters.API_Token),
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
        # endpoints=[GET_ALL_JOBS_ENDPOINT, GET_JOB_ENDPOINT],
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_candidates,
        # endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
)


RecruiteeJobWarehouse = Warehouse(
    name="Recruitee Jobs",
    data_schema=RecruiteeJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        # endpoints=[SEARCH_JOBS_ENDPOINT],
    ),
    write=WarehouseWriteAction(
        parameters=WriteJobsParameters,
        function=write_jobs,
        # endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
)
