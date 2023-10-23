import typing as t
from logging import LoggerAdapter
from os.path import join

import requests
from pydantic import Field

from hrflow_connectors.connectors.oracleorc.schemas import (
    OracleORCAuth,
    OracleORCHostAndPort,
)
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

_ORACLEORC_EXPAND_DESC = (
    "When this parameter is provided, the specified children are included in the"
    " resource payload (instead of just a link). The value of this query parameter is"
    ' "all" or "". More than one child can be specified using comma as a separator.'
    " Example: ?expand=Employees,Localizations. Nested children can also be provided"
    ' following the format "Child.NestedChild" (Example: ?expand=Employees.Managers).'
    " If a nested child is provided (Example: Employees.Managers), the missing children"
    ' will be processed implicitly. For example, "?expand=Employees.Managers" is the'
    ' same as "?expand=Employees,Employees.Managers" (which will expand Employees and'
    " Managers)."
)

_ORACLEORC_FIELDS_DESC = (
    "This parameter filters the resource fields. Only the specified fields are"
    " returned, which means that if no fields are specified, no fields are returned"
    " (useful to get only the links). If an indirect child resource is provided"
    " (Example: Employees.Managers), the missing children will be processed implicitly."
    ' For example, "?fields=Employees.Managers:Empname" is the same as'
    ' "?fields=;Employees:;Employees.Managers:Empname" (which will only return the'
    ' "Empname" field for Managers). the value of this query parameter is a list of'
    " resource fields. The attribute can be a direct (Example: Employees) or indirect"
    " (Example: Employees.Managers) child. It cannot be combined with expand query"
    " parameter. If both are provided, only fields will be considered. Format:"
    " ?fields=Attribute1,Attribute2. Format for fields in child resource:"
    " ?fields=Accessor1:Attribute1,Attribute2"
)

_ORACLEORC_READ_JOBS_FINDER_DESC = (
    "Used as a predefined finder to search the collection.<br><br>Format"
    " ?finder=\\<finderName>;\\<variableName>=\\<variableValue>,\\<variableName2>=\\<"
    "variableValue2><br><br>The"
    " following are the available finder names and corresponding finder"
    " variables<br><br>  - PrimaryKey Finds job requisitions using a unique ID<br>   "
    " Finder Variables<br>      - SearchId; integer; Finds job requisitions using a"
    " search ID and a primary key<br>  - findReqs Finds job requisitions using find"
    " requests<br>    Finder Variables<br>      - correctedKeyword; string; Corrected"
    " keyword; string; used for searching, based on the spell check result.<br>      -"
    " executeSpellCheckFlag; string; Indicates whether the keyword; string; must be"
    " spell checked. The default value is true.<br>      - facetsList; string; Finds"
    " job requisitions using find requests and facets list<br>      - hotJobFlag;"
    " string; Indicates whether the job requisition is considered a hot job.<br>      -"
    " jobFamilyId; string; Finds job requisitions using find requests and job family"
    " ID<br>      - keyword; string; Finds job requisitions using find requests and"
    " keyword; string;<br>      - lastSelectedFacet; string; Finds job requisitions"
    " using find requests and the last selected facet<br>      - latitude; number;"
    " Finds recruiting job requisitions using find requests and latitude; number;<br>  "
    "    - limit; integer; Finds job requisitions using find requests and limit;"
    " integer;<br>      - location; string; Finds job requisitions using find requests"
    " and location; string;<br>      - locationId; string; Finds job requisitions using"
    " find requests and location; string; ID<br>      - longitude; number; Finds"
    " recruiting job requisitions using find requests and longitude; number;<br>      -"
    " offset; integer; Finds job requisitions using find requests and offset;"
    " integer;<br>      - organizationId; string; Organization ID to find job"
    " requisitions.<br>      - postingEndDate; string; Finds job requisitions using"
    " find requests and the end date of the job posting<br>      - postingStartDate;"
    " string; Finds job requisitions using find requests and the start date of the job"
    " posting<br>      - radius; integer; Finds job requisitions using find requests"
    " and radius; integer;<br>      - radiusUnit; string; Finds job requisitions using"
    " find requests and the unit of the radius; integer;<br>      -"
    " selectedCategoriesFacet; string; Finds job requisitions using find requests and"
    " the selected category facet<br>      - selectedFlexFieldsFacets; string; Finds"
    " job requisitions using selected flexfields facets<br>      -"
    " selectedLocationsFacet; string; Finds job requisitions using find requests and"
    " location; string; facet<br>      - selectedOrganizationsFacet; string; Selected"
    " organization facets to find job requisitions.<br>      -"
    " selectedPostingDatesFacet; string; Finds job requisitions using find requests and"
    " the selected facet of the date of job posting<br>      - selectedTitlesFacet;"
    " string; Finds job requisitions using find requests and title facet<br>      -"
    " selectedWorkLocationsFacet; string; Requisition location; string; ID and unique"
    " ID used to find work locations in a job requisition details preview.<br>      -"
    " selectedWorkplaceTypesFacet; string; Facet of the selected workplace type.<br>   "
    "   - siteNumber; string; Finds recruiting job requisitions using find requests and"
    " the site number<br>      - sortBy; string; Finds job requisitions using find"
    " requests and sort by<br>      - useExactKeywordFlag; string; Indicates whether"
    " the exact keyword; string; is used for searching or if the keyword; string; is"
    " automatically spell checked and corrected. The default value is false.<br>      -"
    " userTargetFacetInputTerm; string; Term used to find facet values for the selected"
    " facet.<br>      - userTargetFacetName; string; Name of the facet to find facet"
    " values.<br>      - workLocationCountryCode; string; Finds job requisitions using"
    " country code of the work location; string;<br>      - workLocationZipCode;"
    " string; Finds job requisitions using the ZIP code of the work location;"
    " string;<br>  - findReqsByQRShortCode Finds job requisitions by QR short code.<br>"
    "      Finder Variables<br>      - limit; integer; The maximum number of job"
    " requisitions displayed per page.<br>      - offset; integer; The offset; integer;"
    " value to get job requisitions from the result set.<br>      - qrShortCode;"
    " string; The short code associated with the QR code.<br>      - siteNumber;"
    " string; The site number associated with the short code URL.<br>  -"
    " findSimilarRequisitions Finds similar jobs<br>    Finder Variables<br>      -"
    " candidateNumber; string; Finds similar jobs using candidate ID<br>      - limit;"
    " integer; Finds job requisitions using find requests and limit; integer;<br>     "
    " - offset; integer; Finds job requisitions using find requests and offset;"
    " integer;<br>      - requisitionId; integer; Finds similar jobs using job ID<br>  "
    "    - siteNumber; string; Finds similar jobs using site number<br>  -"
    " findSimilarRequisitionsByCandidate Finds jobs by candidate.<br>    Finder"
    " Variables<br>      - candidateNumber; string; Finds similar jobs using candidate"
    " ID<br>      - limit; integer; Finds job requisitions using find requests and"
    " limit; integer;<br>      - offset; integer; Finds job requisitions using find"
    " requests and offset; integer;<br>      - requisitionId; integer; Finds similar"
    " jobs using job ID<br>      - siteNumber; string; Finds similar jobs using site"
    " number"
)

_ORACLEORC_LIMIT_DESC = (
    "This parameter restricts the number of resources returned inside the resource"
    " collection. If the limit exceeds the resource count then the framework will only"
    " return the available resources."
)

_ORACLEORC_LINKS_DESC = (
    "This parameter can be used to show only certain links while accessing a singular"
    " resource or a resource collection. The parameter value format is a"
    " comma-separated list of : \\<link_relation>. Example: self,canonical"
)

_ORACLEORC_OFFSET_DESC = (
    "Used to define the starting position of the resource collection. If offset exceeds"
    " the resource count then no resources are returned. Default value is 0."
)

_ORACLEORC_ONLY_DATA_DESC = (
    "The resource item payload will be filtered in order to contain only data (no links"
    " section, for example)."
)

_ORACLEORC_ORDER_BY_DESC = (
    "This parameter orders a resource collection based on the specified fields. The"
    " parameter value is a comma-separated string of attribute names, each optionally"
    ' followed by a colon and "asc" or "desc". Specify "asc" for ascending and "desc"'
    ' for descending. The default value is "asc". For example,'
    " ?orderBy=field1:asc,field2:desc"
)

_ORACLEORC_READ_JOBS_Q_DESC = (
    "This query parameter defines the where clause. The resource collection will be"
    " queried using the provided expressions. The value of this query parameter is one"
    " or more expressions. Example: ?q=Deptno>=10 and <= 30;Loc!=NY<br><br>Format:"
    " ?q=expression1;expression2<br><br>You can use these queryable attributes to"
    " filter this collection resource using the q query parameter:<br>  -"
    " BotQRShortCode; string; The QR short code for the filtered job requisitions that"
    " are displayed.<br>  - CandidateNumber; string; ID assigned to each candidate<br> "
    " - CorrectedKeyword; string; Corrected keyword used for searching, based on the"
    " spell check result.<br>  - ExecuteSpellCheckFlag; boolean; Indicates whether a"
    " spell check was requested for the keyword.<br>  - Facets; string; Facets of the"
    " job requisitions.<br>  - HotJobFlag; boolean; Indicates whether the job"
    " requisition is considered a hot job in a job requisition details preview.<br>  -"
    " JobFamilyId; string; ID assigned to the job family.<br>  - Keyword; string;"
    " Keyword used to search for job requisitions.<br>  - LastSelectedFacet; string;"
    " Last selected facet for retrieving job requisitions.<br>  - Latitude; number;"
    " Latitude of the location of job requisition.<br>  - Limit; integer; Limit of the"
    " job requisition.<br>  - Location; string; Location of the job requisition<br>  -"
    " LocationId; string; ID assigned to the location of the job requisition.<br>  -"
    " Longitude; number; Longitude of the location of the job requisition<br>  -"
    " Offset; integer; Offset of the job requisition<br>  - OrganizationId; string;"
    " Organization ID for a job requisition.<br>  - PostingEndDate; string; End date of"
    " the job posting.<br>  - PostingStartDate; string; Start date of the job"
    " posting.<br>  - Radius; integer; Radius of the job requisition.<br>  -"
    " RadiusUnit; string; Unit of the radius of the job requisition.<br>  -"
    " RequisitionId; integer; ID assigned to the similar job<br>  - SearchId; integer;"
    " Location ID of the secondary locations in a job requisition details preview.<br> "
    " - SelectedCategoriesFacet; string; Facet of the selected categories of the job"
    " requisitions.<br>  - SelectedFlexFieldsFacets; string; Facets of the selected job"
    " requisitions flexfields.<br>  - SelectedLocationsFacet; string; Facet of the"
    " selected locations of the job requisitions.<br>  - SelectedOrganizationsFacet;"
    " string; Facet of the selected organizations of the job requisitions.<br>  -"
    " SelectedPostingDatesFacet; string; Facet of the selected dates of job"
    " posting.<br>  - SelectedTitlesFacet; string; Facet of the selected title of job"
    " requisitions.<br>  - SelectedWorkLocationsFacet; string; Facet of the selected"
    " work locations.<br>  - SelectedWorkplaceTypesFacet; string; Facet of the selected"
    " workplace type.<br>  - SiteNumber; string; Site number of the job"
    " requisition.<br>  - SortBy; string; Sort by attribute for the job"
    " requisition.<br>  - SuggestedKeyword; string; Suggested keyword based on spell"
    " check result.<br>  - TotalJobsCount; integer; Total job count for the job"
    " requisition.<br>  - UseExactKeywordFlag; boolean; Indicates whether the exact"
    " keyword is used for searching or if the keyword is automatically spell checked"
    " and corrected.<br>  - UserTargetFacetInputTerm; string; Term used to filter facet"
    " values.<br>  - UserTargetFacetName; string; Name of the facet for which the value"
    " list must be filtered by the input term.<br>  - WorkLocationCountryCode; string;"
    " Country code of the work location.<br>  - WorkLocationZipCode; string; ZIP code"
    " of the work location."
)

_ORACLEORC_TOTAL_RESULTS_DESC = (
    'The resource collection representation will include the "estimated row count" when'
    ' "?totalResults=true", otherwise the count is not included. The default value is'
    ' "false".'
)

_ORACLEORC_READ_PROFILES_FINDER_DESC = (
    "Used as a predefined finder to search the collection.<br><br>Format"
    " ?finder=\\<finderName>;\\<variableName>=\\<variableValue>,\\<variableName2>=\\<"
    "variableValue2><br><br>The"
    " following are the available finder names and corresponding finder"
    " variables<br><br>  - PrimaryKey Finds all candidates using a unique ID.<br>   "
    " Finder Variables<br>      - CandidateNumber; string; Unique ID and candidate"
    " number used to find candidates.<br>  - findByPersonId Finds all candidates using"
    " a person ID.<br>    Finder Variables<br>      - personId; string; Person ID used"
    " to find candidates.<br>  - search Finds all candidates using search.<br>   "
    " Finder Variables<br>      - keywords; string; Search and keywords; string; used"
    " to find candidates."
)


_ORACLEORC_READ_PROFILES_Q_DESC = (
    "This query parameter defines the where clause. The resource collection will be"
    " queried using the provided expressions. The value of this query parameter is one"
    " or more expressions. Example: ?q=Deptno>=10 and <= 30;Loc!=NY<br><br>Format:"
    " ?q=expression1;expression2<br><br>You can use these queryable attributes to"
    " filter this collection resource using the q query parameter:<br><br>  -"
    " CandLastModifiedDate; string; Candidate Last Modified Date<br>  - CandidateType;"
    " string; Candidate type in recruiting candidates.<br>  - FullName; string; Full"
    " name in recruiting candidates.<br>  - LastName; string; Last name in recruiting"
    " candidates.<br>  - PreferredTimezone; string; Preferred time zone of the"
    " candidate."
)

_ORACLEORC_API_BASE_FURL = "http://{host}:{port}/hcmRestApi/resources/11.13.18.05/"
_ORACLEORC_API_READ_PROFILES_FURL = join(
    _ORACLEORC_API_BASE_FURL, "recruitingCandidates"
)
_ORACLEORC_API_WRITE_PROFILE_FURL = join(
    _ORACLEORC_API_BASE_FURL, "recruitingCandidates"
)
_ORACLEORC_API_READ_JOBS_FURL = join(
    _ORACLEORC_API_BASE_FURL, "recruitingICEJobRequisitions"
)


class OracleORCWriteProfileParameters(ParametersModel):
    authData: OracleORCAuth = Field(..., repr=False, field_type=FieldType.Auth)
    hostAndPort: OracleORCHostAndPort = Field(..., field_type=FieldType.Other)


class OracleORCReadJobsParameters(ParametersModel):
    authData: OracleORCAuth = Field(..., repr=False, field_type=FieldType.Auth)
    hostAndPort: OracleORCHostAndPort = Field(..., field_type=FieldType.Other)
    expand: t.Optional[str] = Field(
        None, description=_ORACLEORC_EXPAND_DESC, field_type=FieldType.QueryParam
    )
    fields: t.Optional[str] = Field(
        None, description=_ORACLEORC_FIELDS_DESC, field_type=FieldType.QueryParam
    )
    finder: t.Optional[str] = Field(
        None,
        description=_ORACLEORC_READ_JOBS_FINDER_DESC,
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        None, description=_ORACLEORC_LIMIT_DESC, field_type=FieldType.QueryParam, ge=1
    )
    links: t.Optional[str] = Field(
        None, description=_ORACLEORC_LINKS_DESC, field_type=FieldType.QueryParam
    )
    offset: t.Optional[int] = Field(
        0, description=_ORACLEORC_OFFSET_DESC, field_type=FieldType.QueryParam, ge=0
    )
    onlyData: t.Optional[bool] = Field(
        None, description=_ORACLEORC_ONLY_DATA_DESC, field_type=FieldType.QueryParam
    )
    orderBy: t.Optional[str] = Field(
        None, description=_ORACLEORC_ORDER_BY_DESC, field_type=FieldType.QueryParam
    )
    q: t.Optional[str] = Field(
        None, description=_ORACLEORC_READ_JOBS_Q_DESC, field_type=FieldType.QueryParam
    )
    totalResults: t.Optional[bool] = Field(
        False,
        description=_ORACLEORC_TOTAL_RESULTS_DESC,
        field_type=FieldType.QueryParam,
    )

    class Config:
        validate_assignment = True


class OracleORCReadProfilesParameters(ParametersModel):
    authData: OracleORCAuth = Field(..., repr=False, field_type=FieldType.Auth)
    hostAndPort: OracleORCHostAndPort = Field(..., field_type=FieldType.Other)
    expand: t.Optional[str] = Field(
        None, description=_ORACLEORC_EXPAND_DESC, field_type=FieldType.QueryParam
    )
    fields: t.Optional[str] = Field(
        None, description=_ORACLEORC_FIELDS_DESC, field_type=FieldType.QueryParam
    )
    finder: t.Optional[str] = Field(
        None,
        description=_ORACLEORC_READ_PROFILES_FINDER_DESC,
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        None, description=_ORACLEORC_LIMIT_DESC, field_type=FieldType.QueryParam, ge=1
    )
    links: t.Optional[str] = Field(
        None, description=_ORACLEORC_LINKS_DESC, field_type=FieldType.QueryParam
    )
    offset: t.Optional[int] = Field(
        0, description=_ORACLEORC_OFFSET_DESC, field_type=FieldType.QueryParam, ge=0
    )
    onlyData: t.Optional[bool] = Field(
        None, description=_ORACLEORC_ONLY_DATA_DESC, field_type=FieldType.QueryParam
    )
    q: t.Optional[str] = Field(
        None,
        description=_ORACLEORC_READ_PROFILES_Q_DESC,
        field_type=FieldType.QueryParam,
    )
    totalResults: t.Optional[bool] = Field(
        False,
        description=_ORACLEORC_TOTAL_RESULTS_DESC,
        field_type=FieldType.QueryParam,
    )

    class Config:
        validate_assignment = True


def write_profile(
    adapter: LoggerAdapter,
    parameters: OracleORCWriteProfileParameters,
    profiles: t.Iterable[t.Dict],
):
    """
    Write `HrFlow.Profile` as candidate for a `OracleORC.JobRequisition`

    Returns:
      list of dictionaries representing the profiles for which the action has failed
    """

    failed_profiles = []

    headers = {"Content-Type": "application/json"}
    url = _ORACLEORC_API_WRITE_PROFILE_FURL.format(
        host=parameters.hostAndPort.host,
        port=parameters.hostAndPort.port,
    )

    for profile in list(profiles):
        response = requests.post(url, headers=headers, json=profile)
        if response.status_code != requests.codes.created:
            failed_profiles.append(profile)

    return failed_profiles


def generic_read(
    is_job: bool = True,
) -> t.Callable[
    [
        LoggerAdapter,
        t.Union[OracleORCReadJobsParameters, OracleORCReadProfilesParameters],
        t.Optional[ReadMode],
        t.Optional[str],
    ],
    t.Iterable[t.Dict],
]:
    def _read(
        adapter: LoggerAdapter,
        parameters: (
            OracleORCReadJobsParameters if is_job else OracleORCReadProfilesParameters
        ),
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ):
        """
        Read Job Requisitions (resp. Candidates) from Oracle ORC into a HrFlow board (\
 resp. source)

        Returns:
          Iterable of dictionaries representing the respective object
        """

        auth = parameters.authData
        params = parameters.dict(exclude=["auth", "hostAndPort"])
        url = (
            _ORACLEORC_API_READ_JOBS_FURL
            if is_job
            else _ORACLEORC_API_READ_PROFILES_FURL
        ).format(host=parameters.hostAndPort.host, port=parameters.hostAndPort.port)

        while True:
            response = requests.get(url, params=params, auth=auth)

            if response.status_code != requests.codes.ok:
                adapter.error(
                    "Failed to pull from Oracle ORC with"
                    f" status_code={response.status_code}, url={url},"
                    f" params={params} and response={response.text}"
                )
                raise RuntimeError("Failed to pull from Oracle ORC")

            response_json = response.json()
            items = response_json.get("items")

            if not items:
                break

            if not isinstance(items, list):
                yield items
            else:
                for item in items:
                    yield item

            if not response_json.get("hasMore"):
                break

            params["offset"] += response_json["count"]

    return _read


OracleORCProfilesWarehouse = Warehouse(
    name="OracleORC Profiles",
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=OracleORCWriteProfileParameters,
        function=write_profile,
        endpoints=[
            ActionEndpoints(
                name="Create a candidate",
                url=_ORACLEORC_API_WRITE_PROFILE_FURL,
                description="",
            )
        ],
    ),
    read=WarehouseReadAction(
        parameters=OracleORCReadProfilesParameters,
        function=generic_read(is_job=False),
        endpoints=[
            ActionEndpoints(
                name="Get all recruiting candidates",
                url=_ORACLEORC_API_READ_PROFILES_FURL,
                description="",
            )
        ],
    ),
)

OracleORCJobsWarehouse = Warehouse(
    name="OracleORC Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=OracleORCReadJobsParameters,
        function=generic_read(is_job=True),
        endpoints=[
            ActionEndpoints(
                name="Get all job requisitions",
                url=_ORACLEORC_API_READ_JOBS_FURL,
                description="",
            )
        ],
    ),
)
