import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.adzuna.schemas import AdzunaJob
from hrflow_connectors.core import Warehouse, WarehouseReadAction
from hrflow_connectors.core.warehouse import ActionEndpoints

ADZUNA_ENDPOINT = "http://api.adzuna.com/v1/api"
SEARCH_JOBS_ENDPOINT = ActionEndpoints(
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


class ReadJobsParameters(BaseModel):
    country: CountryCode = Field(
        ..., description="ISO 8601 country code of the country of interes"
    )
    app_id: str = Field(
        ..., description="Application ID, supplied by Adzuna", repr=False
    )
    app_key: str = Field(
        ..., description="Application key, supplied by Adzuna", repr=False
    )
    # page: int = Field(..., description="Page number")
    results_per_page: int = Field(
        None,
        description="The number of results to include on a page of search results.",
    )
    what: str = Field(
        None,
        description=(
            "The keywords to search for. Multiple terms may be space separated."
        ),
    )
    what_and: str = Field(
        None, description="The keywords to search for, all keywords must be found."
    )
    what_phrase: str = Field(
        None,
        description="An entire phrase which must be found in the description or title.",
    )
    what_or: str = Field(
        None,
        description=(
            "The keywords to search for, any keywords may be found. Multiple terms may"
            " be space separated."
        ),
    )
    what_exclude: str = Field(
        None,
        description=(
            "Keywords to exclude from the search. Multiple terms may be space"
            " separated."
        ),
    )
    title_only: str = Field(
        None,
        description=(
            "Keywords to find, but only in the title. Multiple terms may be space"
            " separated."
        ),
    )
    where: str = Field(
        None,
        description=(
            "The geographic centre of the search. Place names, postal codes, etc. may"
            " be used.	"
        ),
    )
    distance: int = Field(
        None,
        description=(
            "The distance in kilometres from the centre of the place described by the"
            " 'where' parameter. Defaults to 5km."
        ),
    )
    location0: str = Field(
        None,
        description=(
            """The location fields may be used to describe a location,"""
            """ in a similar form to that returned in """
            """a Adzuna::API::Response::Location object.For example,"""
            """ "location0=UK&location1=South East England&location2=Surrey" """
            """ will performn a search over the county of Surrey."""
        ),
    )
    location1: t.Optional[str]
    location2: t.Optional[str]
    location3: t.Optional[str]
    location4: t.Optional[str]
    location5: t.Optional[str]
    location6: t.Optional[str]
    location7: t.Optional[str]
    max_days_old: int = Field(
        None,
        description="The age of the oldest advertisment in days that will be returned.",
    )
    category: str = Field(
        None, description='The category tag, as returned by the "category" endpoint.'
    )
    sort_dir: SortDir = Field(
        None, description="The direction to order the search results."
    )
    sort_by: SortKey = Field(None, description="The ordering of the search results.")
    salary_min: int = Field(
        None, description="The minimum salary we wish to get results for."
    )
    salary_max: int = Field(
        None, description="The maximum salary we wish to get results for."
    )
    salary_include_unknown: Filter = Field(
        None, description="""If set it "1", jobs without a known salary are returned."""
    )
    full_time: Filter = Field(
        None, description="""If set to "1", only full time jobs will be returned."""
    )
    part_time: Filter = Field(
        None, description="""If set to "1", only part time jobs will be returned."""
    )
    contract: Filter = Field(
        None, description="""If set to "1", only contract jobs will be returned."""
    )
    permanent: Filter = Field(
        None, description="""If set to "1", only permanent jobs will be returned."""
    )
    company: str = Field(
        None,
        description=(
            "The canonical company name. This may be returned in a"
            " Adzuna::API::Response::Company object when a job is returned. A full list"
            " of allowed terms in not available through the API."
        ),
    )


def read(adapter: LoggerAdapter, parameters: ReadJobsParameters) -> t.Iterable[t.Dict]:
    params = parameters.dict()
    page = 1
    country = params["country"]
    del params["country"]

    iterate_over_pages = True

    while iterate_over_pages:
        ADZUNA_JOBS_SEARCH_ENDPOINT = (
            f"""{ADZUNA_ENDPOINT}/jobs/{country}/search/{page}"""
        )
        # del params["page"]
        response = requests.get(
            ADZUNA_JOBS_SEARCH_ENDPOINT,
            headers={},
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

        if jobs:
            for job in jobs:
                yield job
        else:
            iterate_over_pages = False

        page += 1
        if page == MAX_NUMBER_OF_PAGES:
            break


# TODO: Warehouse

AdzunaJobWarehouse = Warehouse(
    name="Adzuna Jobs",
    data_schema=AdzunaJob,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[SEARCH_JOBS_ENDPOINT],
    ),
)
