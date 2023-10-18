import json
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.ceipal.schemas import (
    Applicants,
    JobPostingsDetails,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)

MAX_ITERATIONS = 10


class BaseParameters(ParametersModel):
    email: str = Field(
        ...,
        description="Email of the user to authenticate",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Password of the user to authenticate",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_key: str = Field(
        ...,
        description="API key of the user to authenticate",
        repr=False,
        field_type=FieldType.Auth,
    )


class JobPostingsParameters(BaseParameters):
    limit: int = Field(
        20,
        description="Default page limit is 20 and can go up to 50 records per page.",
        field_type=FieldType.QueryParam,
    )
    business_unit_id: int = Field(
        None,
        description=(
            "Every job is associated with a business unit. Pass the business unit id as"
            " the parameter to get the jobs."
        ),
        field_type=FieldType.QueryParam,
    )
    country: int = Field(
        None,
        description=(
            "Pull the jobs based on the country. Use the countries endpoint to get the"
            " list of countries."
        ),
        field_type=FieldType.QueryParam,
    )
    state: int = Field(
        None,
        description=(
            "Pass the state id as the parameter to get the jobs. Use the states"
            " endpoint to get the states list."
        ),
        field_type=FieldType.QueryParam,
    )
    job_status: int = Field(
        None,
        description=(
            "Use the job status endpoint from the master data to get the job statuses."
            " Pass the id here to pull the matching jobs."
        ),
        field_type=FieldType.QueryParam,
    )
    post_on_careerportal: bool = Field(
        None,
        description=(
            "Send 1 to get all the jobs that are posted on the careers page. 0 for the"
            " jobs that are not posted."
        ),
        field_type=FieldType.QueryParam,
    )
    fromdate: str = Field(
        None,
        description=(
            "To get the jobs in between the dates, use this parameter (date format:"
            " mm-dd-yyyy)"
        ),
        field_type=FieldType.QueryParam,
    )
    todate: str = Field(
        None,
        description=(
            "This parameter works along with the fromdate. Gives the jobs that are"
            " created between the from date and to date (date format: mm-dd-yyyy)"
        ),
        field_type=FieldType.QueryParam,
    )
    filter: str = Field(
        None,
        description=(
            "When the from date and to dates are used, this parameter is mandatory. Use"
            " ‘created’ as the filter value to get the jobs that are created between"
            " the from and to dates."
        ),
        field_type=FieldType.QueryParam,
    )
    posted_ago_days: int = Field(
        None,
        description=(
            "Pass any numeric value to get the jobs that are created within this number"
            " of days."
        ),
        field_type=FieldType.QueryParam,
    )
    sortorder: str = Field(
        None,
        description="Use either asc or desc to sort the job postings list.",
        field_type=FieldType.QueryParam,
    )
    sortby: str = Field(
        None,
        description=(
            "This filter is used along with the sortorder. Job postings can be sorted"
            " based on the job code, created date, modified date."
        ),
        field_type=FieldType.QueryParam,
    )
    assigned_recruiter: dict = Field(
        None,
        description="Pass the assigned recruiter as a dictionary.",
        field_type=FieldType.QueryParam,
    )


class ReadApplicantsParameters(BaseParameters):
    limit: int = Field(
        20,
        description="Default page limit is 20 and can go up to 50 records per page.",
        field_type=FieldType.QueryParam,
    )
    created_by: int = Field(
        None,
        description="User ID of the applicant creator",
        field_type=FieldType.QueryParam,
    )
    source: int = Field(
        None,
        description="Source ID of the applicants",
        field_type=FieldType.QueryParam,
    )
    applicant_status: int = Field(
        None,
        description="Applicant status ID",
        field_type=FieldType.QueryParam,
    )
    sortorder: str = Field(
        None,
        description="Sort order (asc or desc)",
        field_type=FieldType.QueryParam,
    )
    sortby: str = Field(
        None,
        description="Sort by field (e.g., applicant_id)",
        field_type=FieldType.QueryParam,
    )


def read_jobs(
    adapter: LoggerAdapter,
    parameters: JobPostingsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    jobs_list_url = "https://api.ceipal.com/v1/getJobPostingsList"
    adapter.info("Fetching jobs from Ceipal")
    adapter.info("getting access token")
    token, refresh_token = get_access_token(parameters)
    if token is None:
        raise Exception("Authentication failed")
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {token}"}
    params = {
        "limit": parameters.limit,
        "offset": 0,
        "business_unit_id": parameters.business_unit_id,
        "country": parameters.country,
        "state": parameters.state,
        "job_status": parameters.job_status,
        "post_on_careerportal": parameters.post_on_careerportal,
        "fromdate": parameters.fromdate,
        "todate": parameters.todate,
        "filter": parameters.filter,
        "posted_ago_days": parameters.posted_ago_days,
        "sortorder": parameters.sortorder,
        "sortby": parameters.sortby,
        "assigned_recruiter": parameters.assigned_recruiter,
    }
    iterations = 0
    while iterations < MAX_ITERATIONS:
        response = requests.get(jobs_list_url, headers=headers, params=params)
        if response.status_code == 200:
            result = response.json()
            if not result:
                break
            for job in result:
                yield job
            params["offset"] += parameters.limit
            iterations += 1  # Increment the iteration count
        elif response.status_code == 403:
            token = refresh_token(refresh_token)
            headers["Authorization"] = f"Bearer {token}"  # Update the token in headers
        else:
            raise Exception("Error in fetching jobs")


def refresh_token(refresh_token: str):
    url = "https://api.ceipal.com/v1/refreshToken?Token=string"
    headers = {"Content-Type": "application/json", "Token": "Bearer {access_token}"}
    response = requests.request("POST", url, headers=headers)
    return response.text


def read_applicants(
    adapter: LoggerAdapter,
    parameters: ReadApplicantsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    applicants_list_url = "https://api.ceipal.com/v1/getApplicantsList"
    adapter.info("Fetching applicants from Ceipal")
    adapter.info("getting access token")
    token, refresh_token = get_access_token(parameters)
    if token is None:
        raise Exception("Authentication failed")
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {token}"}
    params = {
        "limit": parameters.limit,
        "offset": 0,
        "created_by": parameters.created_by,
        "source": parameters.source,
        "applicant_status": parameters.applicant_status,
        "sortorder": parameters.sortorder,
        "sortby": parameters.sortby,
    }
    iterations = 0
    while iterations < MAX_ITERATIONS:
        response = requests.get(applicants_list_url, headers=headers, params=params)
        if response.status_code == 200:
            result = response.json()
            if not result:
                break
            for applicant in result:
                yield applicant
            params["offset"] += parameters.limit
            iterations += 1  # Increment the iteration count
        elif response.status_code == 403:
            token = refresh_token(refresh_token)
            headers["Authorization"] = f"Bearer {token}"  # Update the token in headers
        else:
            raise Exception("Error in fetching applicants")


def get_access_token(parameters: BaseParameters):
    url = "https://api.ceipal.com/v1/createAuthtoken"
    payload = json.dumps(
        {
            "username": parameters.email,
            "password": parameters.password,
            "api_key": parameters.api_key,
        }
    )
    headers = {"Content-Type": "application/json"}
    response = requests.request("POST", url, headers=headers, data=payload)
    if response.status_code != 200:
        raise Exception("Authentication failed")
    token = response.json().get("access_token")
    refresh_token = response.json().get("refresh_token")

    return token, refresh_token


CeipalJobWarehouse = Warehouse(
    name="Ceipal Jobs",
    data_schema=JobPostingsDetails,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=JobPostingsParameters,
        function=read_jobs,
    ),
)

CeipalProfileWarehouse = Warehouse(
    name="Ceipal Profiles",
    data_schema=Applicants,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadApplicantsParameters,
        function=read_applicants,
    ),
)
