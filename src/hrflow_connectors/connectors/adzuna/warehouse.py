import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.adzuna.schemas import AdzunaJob
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)
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


class ReadJobsParameters(ParametersModel):
    country: CountryCode = Field(
        ...,
        description="ISO 8601 country code of the country of interest",
        field_type=FieldType.QueryParam,
    )
    app_id: str = Field(
        ...,
        description="Application ID, supplied by Adzuna",
        repr=False,
        field_type=FieldType.Auth,
    )
    app_key: str = Field(
        ...,
        description="Application key, supplied by Adzuna",
        repr=False,
        field_type=FieldType.Auth,
    )
    results_per_page: int = Field(
        None,
        description="The number of results to include on a page of search results.",
        field_type=FieldType.QueryParam,
    )
    what: str = Field(
        None,
        description=(
            "The keywords to search for. Multiple terms may be space separated."
        ),
        field_type=FieldType.QueryParam,
    )
    what_and: str = Field(
        None,
        description="The keywords to search for, all keywords must be found.",
        field_type=FieldType.QueryParam,
    )
    what_phrase: str = Field(
        None,
        description="An entire phrase which must be found in the description or title.",
        field_type=FieldType.QueryParam,
    )
    what_or: str = Field(
        None,
        description=(
            "The keywords to search for, any keywords may be found. Multiple terms may"
            " be space separated."
        ),
        field_type=FieldType.QueryParam,
    )
    what_exclude: str = Field(
        None,
        description=(
            "Keywords to exclude from the search. Multiple terms may be space"
            " separated."
        ),
        field_type=FieldType.QueryParam,
    )
    title_only: str = Field(
        None,
        description=(
            "Keywords to find, but only in the title. Multiple terms may be space"
            " separated."
        ),
        field_type=FieldType.QueryParam,
    )
    where: str = Field(
        None,
        description=(
            "The geographic centre of the search. Place names, postal codes, etc. may"
            " be used.	"
        ),
        field_type=FieldType.QueryParam,
    )
    distance: int = Field(
        None,
        description=(
            "The distance in kilometres from the centre of the place described by the"
            " 'where' parameter. Defaults to 5km."
        ),
        field_type=FieldType.QueryParam,
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
        field_type=FieldType.QueryParam,
    )
    location1: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location2: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location3: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location4: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location5: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location6: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    location7: t.Optional[str] = Field(
        field_type=FieldType.QueryParam,
    )
    max_days_old: int = Field(
        None,
        description="The age of the oldest advertisment in days that will be returned.",
        field_type=FieldType.QueryParam,
    )
    category: str = Field(
        None,
        description='The category tag, as returned by the "category" endpoint.',
        field_type=FieldType.QueryParam,
    )
    sort_dir: SortDir = Field(
        None,
        description="The direction to order the search results.",
        field_type=FieldType.QueryParam,
    )
    sort_by: SortKey = Field(
        None,
        description="The ordering of the search results.",
        field_type=FieldType.QueryParam,
    )
    salary_min: int = Field(
        None,
        description="The minimum salary we wish to get results for.",
        field_type=FieldType.QueryParam,
    )
    salary_max: int = Field(
        None,
        description="The maximum salary we wish to get results for.",
        field_type=FieldType.QueryParam,
    )
    salary_include_unknown: Filter = Field(
        None,
        description="""If set it "1", jobs without a known salary are returned.""",
        field_type=FieldType.QueryParam,
    )
    full_time: Filter = Field(
        None,
        description="""If set to "1", only full time jobs will be returned.""",
        field_type=FieldType.QueryParam,
    )
    part_time: Filter = Field(
        None,
        description="""If set to "1", only part time jobs will be returned.""",
        field_type=FieldType.QueryParam,
    )
    contract: Filter = Field(
        None,
        description="""If set to "1", only contract jobs will be returned.""",
        field_type=FieldType.QueryParam,
    )
    permanent: Filter = Field(
        None,
        description="""If set to "1", only permanent jobs will be returned.""",
        field_type=FieldType.QueryParam,
    )
    company: str = Field(
        None,
        description=(
            "The canonical company name. This may be returned in a"
            " Adzuna::API::Response::Company object when a job is returned. A full list"
            " of allowed terms in not available through the API."
        ),
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    params = parameters.dict()
    page = 1
    country = params.pop("country")

    while True:
        ADZUNA_JOBS_SEARCH_ENDPOINT = "{}/jobs/{}/search/{}".format(
            ADZUNA_ENDPOINT, country.value, page
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


AdzunaJobWarehouse = Warehouse(
    name="Adzuna Jobs",
    data_schema=AdzunaJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[SEARCH_JOBS_ENDPOINT],
    ),
)
