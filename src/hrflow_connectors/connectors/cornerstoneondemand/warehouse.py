import typing as t
from datetime import datetime
from logging import LoggerAdapter
from time import time

import requests
from pydantic import Field

from hrflow_connectors.connectors.cornerstoneondemand.schemas import (
    CornerstoneOnDemandApplicationPreferences,
    CornerstoneOnDemandAuthentication,
    CornerstoneOnDemandCandidatePreferences,
    CornerstoneOnDemandQuestion,
    CornerstoneOnDemandSource,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.auth import (
    _auth_data_field_get,
    _token_limit_and_headers_get,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.enums import (
    CornerstoneOnDemandEndpoint,
    CornerstoneOnDemandEnv,
    CornerstoneOnDemandScope,
    CornerstoneOnDemandStatus,
    CornerstoneOnDemandSupportedISOLanguageCode,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.tools import (
    _api_formattable_url_get,
    _format_datetime,
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

_CORNERSTONE_JOB_REQUISITION_ENDPOINT_DESCRIPTION = (
    "This API allows you to retrieve job requisition details from CornerstoneOnDemand"
    " Recruiting. The API supports retrieving a single job requisition as well as using"
    " query filters to retrieve multiple job requisitions in a single call. If you"
    " are using the lastModifiedSince query parameter, please note that this"
    " parameter does not account for changes made to job postings. It only accounts for"
    "   changes made to the job requisition itself. Any changes made in the Create/Edit"
    " Job Requisition General page are accounted for by the lastModifiedSince filter."
    " The FromDate and ToDate parameters allow you to search for job requisition by"
    " initial creation date. The API does not include requisition custom fields in the"
    " response. To retrieve custom fields, please use the `Get Job Requisition Custom"
    " Field` API. The API does not resolve any job ad tags to their actual values in"
    " the ExternalAd, InternalAd, and MobileAd response fields. In order to retrieve"
    " resolved job ads, please use the `Get Job Requisition Ad Details` API. To"
    " learn more about job ad tags in CornerstoneOnDemand Recruiting, please see this"
    " Online Help article. You must be logged in to your CornerstoneOnDemand portal"
    " to access this article."
)

_CORNERSTONE_CANDIDATES_ENDPOINT_DESCRIPTION = (
    "In order to post an application, certain fields must be sent over as part of the"
    " application. The POST Candidate and Application endpoint is not dependent on the"
    " GET Application Workflow endpoint. This endpoint can be used to create a"
    " candidate and submit an application. Considerations: If an application is posted"
    " with a required Custom Integration, standard action item, integration, or"
    " additional attachment, the application will be posted in an incomplete status."
    " Resumes added via the POST Candidate and Application endpoint are not parsed. If"
    " an additional attachment is posted using the POST Candidate and Application"
    " endpoint, the entire additional attachment section will be considered complete,"
    " even if more than one additional attachment is marked required in the application"
    " workflow."
)

_CORNERSTONE_JOB_APPLICANT_ENDPOINT_DESCRIPTION = (
    "This API allows you to retrieve job applicants in a given status. Note that this"
    " API does not include applicant custom fields in the response. To retrieve"
    " applicant custom fields, please use the Get Job Applicant with Custom Fields API."
)


class CornerstoneOnDemandReadJobsParameters(ParametersModel):
    authData: CornerstoneOnDemandAuthentication = _auth_data_field_get()
    Title: t.Optional[str] = Field(
        None,
        description="Job Title.",
        field_type=FieldType.QueryParam,
    )
    ReqId: t.Optional[str] = Field(
        None,
        description="Requisition Id.",
        field_type=FieldType.QueryParam,
    )
    FromDate: t.Optional[datetime] = Field(
        None,
        description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.",
        field_type=FieldType.QueryParam,
    )
    ToDate: t.Optional[datetime] = Field(
        None,
        description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.",
        field_type=FieldType.QueryParam,
    )
    lastModifiedSince: t.Optional[datetime] = Field(
        None,
        description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss.",
        field_type=FieldType.QueryParam,
    )
    DivisionId: t.Optional[str] = Field(
        None,
        description="Valid Division Id.",
        field_type=FieldType.QueryParam,
    )
    LocationId: t.Optional[str] = Field(
        None,
        description="Valid Location Id.",
        field_type=FieldType.QueryParam,
    )
    Statuses: t.Optional[t.List[CornerstoneOnDemandStatus]] = Field(
        None,
        description="Comma separated list of statuses. e.g. 'Draft,Open,Closed'.",
        field_type=FieldType.QueryParam,
    )
    Language: t.Optional[CornerstoneOnDemandSupportedISOLanguageCode] = Field(
        None,
        description=(
            "Language should include ISO language code. Example en-US, fr-FR, it-IT,"
            " en-GB..."
        ),
        field_type=FieldType.QueryParam,
    )


class CornerstoneOnDemandWriteProfileParameters(ParametersModel):
    authData: CornerstoneOnDemandAuthentication = _auth_data_field_get()
    applicationPreferences: t.Optional[
        CornerstoneOnDemandApplicationPreferences
    ] = Field(CornerstoneOnDemandApplicationPreferences(), field_type=FieldType.Other)
    candidatePreferences: t.Optional[CornerstoneOnDemandCandidatePreferences] = Field(
        None, field_type=FieldType.Other
    )
    jobRequisitionId: str = Field(
        ...,
        description=(
            'The ATS job requisition\'s identifier. This is a "ref" value and not the'
            ' internal "id". A correct example is Req123.'
        ),
        field_type=FieldType.Other,
    )
    questions: t.Optional[t.List[CornerstoneOnDemandQuestion]] = Field(
        None,
        description=(
            "A collection of application submission data for questions of type:"
            " Disclaimer, Compliance and Prescreening."
        ),
        field_type=FieldType.Other,
    )
    source: t.Optional[CornerstoneOnDemandSource] = Field(
        None, field_type=FieldType.Other
    )


class CornerstoneOnDemandReadProfilesParameters(ParametersModel):
    authData: CornerstoneOnDemandAuthentication = _auth_data_field_get()
    CurrentStatus: CornerstoneOnDemandStatus = Field(
        ..., description="Valid Status.", field_type=FieldType.QueryParam
    )
    StatusDate: t.Optional[datetime] = Field(
        None,
        description="Local Datetime Value. Format should be yyyy-mm-dd.",
        field_type=FieldType.QueryParam,
    )
    CsodGUID: t.Optional[str] = Field(
        None, description="Applicant's GUID Value.", field_type=FieldType.QueryParam
    )
    FirstName: t.Optional[str] = Field(
        None, description="Applicant's First Name.", field_type=FieldType.QueryParam
    )
    LastName: t.Optional[str] = Field(
        None, description="Applicant's Last Name.", field_type=FieldType.QueryParam
    )
    RequisitionID: t.Optional[str] = Field(
        None, description="Job Requisition Id.", field_type=FieldType.QueryParam
    )
    JobTitle: t.Optional[str] = Field(
        None, description="Job Requision Title.", field_type=FieldType.QueryParam
    )


def generic_read_factory(
    is_job: bool = True,
) -> t.Callable[
    [
        LoggerAdapter,
        t.Union[
            CornerstoneOnDemandReadJobsParameters,
            CornerstoneOnDemandReadProfilesParameters,
        ],
        t.Optional[ReadMode],
        t.Optional[str],
    ],
    t.Iterable[t.Dict],
]:
    def _read(
        adapter: LoggerAdapter,
        parameters: (
            CornerstoneOnDemandReadJobsParameters
            if is_job
            else CornerstoneOnDemandReadProfilesParameters
        ),
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
        """
        Reads jobs (resp. candidates) from CornerstoneOnDemand into a HrFlow board
        (resp. source).

        Returns:
          Iterable of dictionaries representing the respective object.
        """

        token_limit = 0  # init value
        auth = parameters.authData
        name = "REQUISITION" if is_job else "APPLICANT"
        params = parameters.dict(exclude={"authData"}, exclude_none=None)
        params.update(dict(Page=1, PageSize=100))

        # @pydantic.field_serializer is not available in the current poetry pydantic
        # version (1.10.8)
        if is_job:
            for key in ["FromDate", "lastModifiedSince", "ToDate"]:
                if params.get(key):
                    params[key] = _format_datetime(params[key], include_time=True)
            if params.get("Statuses"):
                params["Statuses"] = ",".join(params["Statuses"])
        else:  # candidate
            if params.get("StatusDate"):
                params["StatusDate"] = _format_datetime(params["StatusDate"])

        url = _api_formattable_url_get(
            CornerstoneOnDemandEndpoint[f"JOB_{name}"]
        ).format(corpname=auth.corpname, env=CornerstoneOnDemandEnv.PRODUCTION)

        while True:
            if time() >= token_limit:
                token_limit, headers = _token_limit_and_headers_get(
                    auth, [CornerstoneOnDemandScope[f"JOB_{name}_READ"]]
                )

            response = requests.get(url, headers=headers, params=params)

            if response.status_code != requests.codes.ok:
                adapter.error(
                    "Failed to pull from CornerstoneOnDemand with"
                    f" status_code={response.status_code}, url={url},"
                    f" params={params} and response={response.text}"
                )
                raise RuntimeError(f'{"Failed to pull from CornerstoneOnDemand"}')
            else:
                data = response.json()["data"]
                if isinstance(data, list):
                    for record in data:
                        yield record
                    if len(data) < params["PageSize"]:
                        break
                else:
                    yield data

            params["Page"] += 1

    return _read


def write_profile(
    adapter: LoggerAdapter,
    parameters: CornerstoneOnDemandWriteProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    """
    Writes `HrFlow.Profile` as candidate for a `CornerstoneOnDemand.Job`.

    Returns:
      List of dictionaries representing the profiles for which the action has failed
    """

    failed_items = []
    token_limit = 0  # init value
    auth = parameters.authData
    enrich_params = parameters.dict(exclude={"authData"}, exclude_none=True)

    url = _api_formattable_url_get(CornerstoneOnDemandEndpoint.CANDIDATES).format(
        corpname=parameters.authData.corpname, env=CornerstoneOnDemandEnv.PRODUCTION
    )

    for item in list(items):
        if time() >= token_limit:
            token_limit, headers = _token_limit_and_headers_get(
                auth, [CornerstoneOnDemandEndpoint.CANDIDATES]
            )
        item.update(enrich_params)
        response = requests.post(url, headers=headers, json=item)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to CornerstoneOnDemand with"
                f" status_code={response.status_code}, url={url}, item={item} and"
                f" reponse={response.text}"
            )
            failed_items.append(item)

    return failed_items


CornerstoneOnDemandJobsWarehouse = Warehouse(
    name="CornerstoneOnDemand Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=CornerstoneOnDemandReadJobsParameters,
        function=generic_read_factory(is_job=True),
        endpoints=[
            ActionEndpoints(
                name="Get Job Requisition",
                url=_api_formattable_url_get(
                    CornerstoneOnDemandEndpoint.JOB_REQUISITION
                ),
                description=_CORNERSTONE_JOB_REQUISITION_ENDPOINT_DESCRIPTION,
            )
        ],
    ),
)

CornerstoneOnDemandProfilesWarehouse = Warehouse(
    name="CornerstoneOnDemand Profiles",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=CornerstoneOnDemandReadProfilesParameters,
        function=generic_read_factory(is_job=False),
        endpoints=[
            ActionEndpoints(
                name="Get Job Applicant",
                url=_api_formattable_url_get(CornerstoneOnDemandEndpoint.JOB_APPLICANT),
                description=_CORNERSTONE_JOB_APPLICANT_ENDPOINT_DESCRIPTION,
            )
        ],
    ),
    write=WarehouseWriteAction(
        parameters=CornerstoneOnDemandWriteProfileParameters,
        function=write_profile,
        endpoints=[
            ActionEndpoints(
                name="Create Candidate and Application",
                url=_api_formattable_url_get(CornerstoneOnDemandEndpoint.CANDIDATES),
                description=_CORNERSTONE_CANDIDATES_ENDPOINT_DESCRIPTION,
            )
        ],
    ),
)
