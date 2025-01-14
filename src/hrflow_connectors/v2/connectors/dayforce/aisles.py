import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.dayforce.schemas import DayforceJobModel
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class AuthParameters(Struct):
    subdomain: Annotated[
        str,
        Meta(
            description="Subdomain used to access Ceridian API",
        ),
    ]

    client_name_space: Annotated[
        str,
        Meta(
            description="Client name space used to access Ceridian API",
        ),
    ]


class ReadJobsParameters(Struct, omit_defaults=True):
    companyName: Annotated[
        t.Optional[str],
        Meta(
            description="Company name. Example: XYZ Co.",
        ),
    ] = None

    parentCompanyName: Annotated[
        t.Optional[str],
        Meta(
            description="Parent Company name. Example: Ceridian",
        ),
    ] = None

    internalJobBoardCode: Annotated[
        t.Optional[str],
        Meta(
            description="XRefCode of Job Board. Example: CANDIDATEPORTAL",
        ),
    ] = None

    includeActivePostingOnly: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "If true, then exclude inactive postings from the result. If False,"
                " then the 'Last Update Time From' and 'Last Update Time To' parameters"
                " are required and the range specified between the 'Last Update Time"
                " From' and 'Last Update Time To' parameters must not be larger than 1"
                " month. Example: True"
            ),
        ),
    ] = None

    lastUpdateTimeFrom: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "A starting timestamp of job posting date. Example: 2017-01-01T13:24:56"
            ),
        ),
    ] = None

    lastUpdateTimeTo: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "An ending timestamp of last updated job posting. Example:"
                " 2017-02-01T13:24:56"
            ),
        ),
    ] = None

    datePostedFrom: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "A starting timestamp of job posting date. Example: 2017-01-01T13:24:56"
            ),
        ),
    ] = None

    datePostedTo: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "An ending timestamp of job posting date. Example: 2017-02-01T13:24:56"
            ),
        ),
    ] = None

    htmlDescription: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "A flag to feed the jobs over with html formatting or plain text"
                " description"
            ),
        ),
    ] = None


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    url = "https://{}.dayforcehcm.com/Api/{}/V1/JobFeeds".format(
        auth_parameters.subdomain, auth_parameters.client_name_space
    )

    response = requests.request(
        "GET",
        url=url,
        headers={},
        data={},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from Ceridian params={} status_code={} response={}"
            .format(params, response.status_code, response.text)
        )
        raise Exception("Failed to pull jobs from Ceridian")
    response = response.json()
    return response


JobsAisle = Aisle(
    name=Entity.job,
    schema=DayforceJobModel,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read,
            update=read,
            archive=read,
        ),
    ),
)
