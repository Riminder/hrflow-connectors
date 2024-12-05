import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.adzuna.schemas import AdzunaJob
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    ReadOperation,
    merge,
)

ADZUNA_BASE_URL = "http://api.adzuna.com/v1/api"
SEARCH_JOBS_ENDPOINT = Endpoint(
    name="Get Adzuna jobs",
    description="Use this endpoint to retrieve Adzuna's job advertisement listings.",
    url="https://api.adzuna.com/v1/doc/Search.md",
)

MAX_NUMBER_OF_PAGES = 10


class CountryCode(str, Enum):
    GB = "gb"
    AT = "at"
    AU = "au"
    BR = "br"
    CA = "ca"
    DE = "de"
    FR = "fr"
    IN = "in"
    IT = "it"
    NL = "nl"
    NZ = "nz"
    PL = "pl"
    RU = "ru"
    SG = "sg"
    US = "us"
    ZA = "za"


class SortDir(str, Enum):
    UP = "up"
    DOWN = "down"


class SortKey(str, Enum):
    DEFAULT = "default"
    HYBRID = "hybrid"
    DATE = "date"
    SALARY = "salary"
    RELEVANCE = "relevance"


class Filter(str, Enum):
    ON = "1"


class AuthParameters(Struct):
    app_id: Annotated[
        t.Optional[str],
        Meta(
            description="Application ID, supplied by Adzuna",
        ),
    ]
    app_key: Annotated[
        t.Optional[str],
        Meta(
            description="Application key, supplied by Adzuna",
        ),
    ]


class ReadJobsParameters(Struct):
    country: Annotated[
        CountryCode,
        Meta(
            description="ISO 8601 country code of the country of interest",
        ),
    ]
    results_per_page: Annotated[
        t.Optional[int],
        Meta(
            description="The number of results to include on a page of search results.",
        ),
    ] = 50
    what: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The keywords to search for. Multiple terms may be space separated."
            ),
        ),
    ] = None
    what_and: Annotated[
        t.Optional[str],
        Meta(
            description="The keywords to search for, all keywords must be found.",
        ),
    ] = None
    what_phrase: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "An entire phrase which must be found in the description or title."
            ),
        ),
    ] = None
    what_or: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The keywords to search for, any keywords may be found. Multiple terms"
                " may be space separated."
            ),
        ),
    ] = None
    what_exclude: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Keywords to exclude from the search. Multiple terms may be space"
                " separated."
            ),
        ),
    ] = None
    title_only: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Keywords to find, but only in the title. Multiple terms may be space"
                " separated."
            ),
        ),
    ] = None
    where: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The geographic centre of the search. Place names, postal codes, etc."
                " may be used.	"
            ),
        ),
    ] = None
    distance: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "The distance in kilometres from the centre of the place described by"
                " the 'where' parameter. Defaults to 5km."
            ),
        ),
    ] = None
    location0: Annotated[
        t.Optional[str],
        Meta(
            description=(
                """The location fields may be used to describe a location,"""
                """ in a similar form to that returned in """
                """a Adzuna::API::Response::Location object.For example,"""
                """ "location0=UK&location1=South East England&location2=Surrey" """
                """ will performn a search over the county of Surrey."""
            ),
        ),
    ] = None
    location1: t.Optional[str] = None

    location2: t.Optional[str] = None

    location3: t.Optional[str] = None

    location4: t.Optional[str] = None

    location5: t.Optional[str] = None

    location6: t.Optional[str] = None

    location7: t.Optional[str] = None

    max_days_old: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "The age of the oldest advertisment in days that will be returned."
            ),
        ),
    ] = None
    category: Annotated[
        t.Optional[str],
        Meta(
            description='The category tag, as returned by the "category" endpoint.',
        ),
    ] = None
    sort_dir: Annotated[
        t.Optional[SortDir],
        Meta(
            description="The direction to order the search results.",
        ),
    ] = None
    sort_by: Annotated[
        t.Optional[SortKey],
        Meta(
            description="The ordering of the search results.",
        ),
    ] = None
    salary_min: Annotated[
        t.Optional[int],
        Meta(
            description="The minimum salary we wish to get results for.",
        ),
    ] = None
    salary_max: Annotated[
        t.Optional[int],
        Meta(
            description="The maximum salary we wish to get results for.",
        ),
    ] = None
    salary_include_unknown: Annotated[
        t.Optional[Filter],
        Meta(
            description="""If set it "1", jobs without a known salary are returned.""",
        ),
    ] = None
    full_time: Annotated[
        t.Optional[Filter],
        Meta(
            description="""If set to "1", only full time jobs will be returned.""",
        ),
    ] = None
    part_time: Annotated[
        t.Optional[Filter],
        Meta(
            description="""If set to "1", only part time jobs will be returned.""",
        ),
    ] = None
    contract: Annotated[
        t.Optional[Filter],
        Meta(
            description="""If set to "1", only contract jobs will be returned.""",
        ),
    ] = None
    permanent: Annotated[
        t.Optional[Filter],
        Meta(
            description="""If set to "1", only permanent jobs will be returned.""",
        ),
    ] = None
    company: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The canonical company name. This may be returned in a"
                " Adzuna::API::Response::Company object when a job is returned. A full"
                " list of allowed terms in not available through the API."
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
    params = asdict(parameters)
    page = 1
    country = params.pop("country")
    params["app_id"] = auth_parameters.app_id
    params["app_key"] = auth_parameters.app_key

    while True:
        ADZUNA_JOBS_SEARCH_ENDPOINT = "{}/jobs/{}/search/{}".format(
            ADZUNA_BASE_URL, country.value, page
        )
        response = requests.get(
            ADZUNA_JOBS_SEARCH_ENDPOINT,
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Adzuna params={}"
                " status_code={} response={}".format(
                    params, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Adzuna")

        jobs = response.json()["results"]
        for job in jobs:
            yield job

        page += 1
        if len(jobs) == 0 or page == MAX_NUMBER_OF_PAGES:
            break


JobsAisle = Aisle(
    name=Entity.job,
    schema=AdzunaJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read, update=read, archive=read),
        endpoints=Endpoints(
            create=SEARCH_JOBS_ENDPOINT,
            update=SEARCH_JOBS_ENDPOINT,
            archive=SEARCH_JOBS_ENDPOINT,
        ),
    ),
)
