import typing as t
from enum import Enum
from logging import LoggerAdapter
from typing import List

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.homerun.schemas import HomerunJob
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class JobStatus(str, Enum):
    public = "public"
    private = "private"
    draft = "draft"
    closed = "closed"


class AuthParameters(Struct):
    public_api_key: Annotated[
        str, Meta(description="The public API key of the Homerun account.")
    ]


class ReadJobsParameters(Struct):
    status_filter: Annotated[
        t.Optional[List[JobStatus]], Meta(description="Name of the vacancy status")
    ] = None


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    headers = {"Authorization  ": f"Bearer {auth_parameters.public_api_key}"}
    if parameters.status_filter:
        filter_string = ",".join(parameters.status_filter)
    params = {
        "filter[status]": filter_string,
        "include": ["location", "department", "total_candidate_count", "page_content"],
        "perPage": 100,
    }
    page = 1
    jobs = []
    while True:
        params["page"] = page
        response = requests.get(
            "https://api.homerun.co/api/v1/vacancies",
            headers=headers,
            params=params,
        )
        if response.status_code != 200:
            adapter.error("Failed to read jobs from Homerun: %s", response.text)
            break
        jobs.extend(response.json()["data"])

        if response.json()["meta"]["last_page"] == page:
            break

    for job in jobs:
        yield job


JobsAisle = Aisle(
    name=Entity.job,
    schema=HomerunJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read, update=read, archive=read),
    ),
)
