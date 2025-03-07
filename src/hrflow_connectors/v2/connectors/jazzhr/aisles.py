import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.jazzhr.schemas import JazzHrApplicants, JazzHrJobs
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

API_URL = "https://api.resumatorapi.com/v1"


class AuthParameters(Struct):
    api_key: Annotated[
        str,
        Meta(
            description="The API key to authenticate with JazzHR",
        ),
    ]


class Status(str, Enum):
    open = "Open"
    on_hold = "On Hold"
    approved = "Approved"
    needs_approval = "Needs Approval"
    drafting = "Drafting"
    filled = "Filled"
    cancelled = "Cancelled"
    closed = "Closed"


class ReadJobsParameters(Struct, omit_defaults=True):
    title: Annotated[
        t.Optional[str],
        Meta(
            description="Title of the Job",
        ),
    ] = None
    recruiter: Annotated[
        t.Optional[str],
        Meta(
            description="Recruiter of the Job",
        ),
    ] = None
    board_code: Annotated[
        t.Optional[str],
        Meta(
            description="Board Code of the Job",
        ),
    ] = None
    department: Annotated[
        t.Optional[str],
        Meta(
            description="Department of the Job",
        ),
    ] = None
    hiring_lead: Annotated[
        t.Optional[str],
        Meta(
            description="Hiring Lead of the Job",
        ),
    ] = None
    state: Annotated[
        t.Optional[str],
        Meta(
            description="Location of Job by State",
        ),
    ] = None
    city: Annotated[
        t.Optional[str],
        Meta(
            description="Location of Job by City",
        ),
    ] = None
    from_open_date: Annotated[
        t.Optional[str],
        Meta(
            description="Filter by job open date. Use YYYY-MM-DD format",
        ),
    ] = None
    to_open_date: Annotated[
        t.Optional[str],
        Meta(
            description="Filter by job open date. Use YYYY-MM-DD format",
        ),
    ] = None
    status: Annotated[
        t.Optional[Status],
        Meta(
            description="Filter by Job Status",
        ),
    ] = None
    confidential: Annotated[
        t.Optional[bool],
        Meta(
            description="Filter by confidentiality",
        ),
    ] = None
    private: Annotated[
        t.Optional[bool],
        Meta(
            description="Filter by privacy",
        ),
    ] = None


class ReadApplicantsParameters(Struct, omit_defaults=True):
    name: Annotated[
        t.Optional[str],
        Meta(
            description="Any substring in first or last name",
        ),
    ] = None
    city: t.Optional[str] = None
    job_id: t.Optional[str] = None
    job_title: t.Optional[str] = None
    recruiter_id: t.Optional[str] = None
    apply_date: Annotated[
        t.Optional[str],
        Meta(
            description="Exact applied date in YYYY-MM-DD format",
        ),
    ] = None
    from_apply_date: Annotated[
        t.Optional[str],
        Meta(
            description="YYYY-MM-DD",
        ),
    ] = None
    to_apply_date: Annotated[
        t.Optional[str],
        Meta(
            description="YYYY-MM-DD",
        ),
    ] = None
    status: Annotated[
        t.Optional[str],
        Meta(
            description="Applicant's workflow status ID",
        ),
    ] = None
    rating: Annotated[
        t.Optional[str],
        Meta(
            description="Applicant's rating (1-5)",
        ),
    ] = None


class WriteApplicantsParameters(Struct):
    pass


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    adapter.debug("Reading jobs from JazzHR")
    # TODO: Recheck how to authenticate with JazzHR
    # TODO: Check if the API key is the only way to authenticate
    # TODO: Check for pagination
    headers = {"Authorization": f"Basic {auth_parameters.api_key}"}
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    response = requests.get("{}/jobs".format(API_URL), headers=headers, params=params)
    if response.status_code == 200:
        jobs = response.json()
        for job in jobs:
            yield job
    else:
        raise Exception(f"Failed to read jobs from JazzHR: {response.text}")


def read_applicants(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadApplicantsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    adapter.debug("Reading applicants from JazzHR")
    # TODO: Recheck how to authenticate with JazzHR
    # TODO: Check if the API key is the only way to authenticate
    # TODO: Check for pagination
    headers = {"Authorization": f"Basic {auth_parameters.api_key}"}
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    response = requests.get(
        "{}/applicants".format(API_URL), headers=headers, params=params
    )

    if response.status_code == 200:
        applicants = response.json()
        for applicant in applicants:
            yield applicant
    else:
        raise Exception(f"Failed to read applicants from JazzHR: {response.text}")


def write_applicants(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteApplicantsParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_applicants = []
    headers = {"Authorization": f"Basic {auth_parameters.api_key}"}
    for item in items:
        response = requests.post(
            "{}/applicants".format(API_URL), headers=headers, json=item
        )
        if response.status_code != 201:
            failed_applicants.append(item)
    return failed_applicants


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=JazzHrApplicants,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadApplicantsParameters,
            update=ReadApplicantsParameters,
            archive=ReadApplicantsParameters,
        ),
        function=merge(
            create=read_applicants,
            update=read_applicants,
            archive=read_applicants,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteApplicantsParameters,
        ),
        function=merge(
            create=write_applicants,
        ),
    ),
)

JobsAisle = Aisle(
    name=Entity.job,
    schema=JazzHrJobs,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
    ),
)
