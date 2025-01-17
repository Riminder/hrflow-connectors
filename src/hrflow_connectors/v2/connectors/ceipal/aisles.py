import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.ceipal.schemas import Applicant, JobPostingDetails
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge

CEIPAL_API_URL = "https://api.ceipal.com/v1"
CEIPAL_ENDPOINT_LIMIT = 50


class AuthParameters(Struct):
    email: Annotated[
        str,
        Meta(
            description="Email of the user to authenticate",
        ),
    ]
    password: Annotated[
        str,
        Meta(
            description="Password of the user to authenticate",
        ),
    ]
    api_key: Annotated[
        str,
        Meta(
            description="API key of the user to authenticate",
        ),
    ]


class ReadJobPostingsParameters(Struct, omit_defaults=True):
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="Number of items to pull from CEIPAL",
        ),
    ] = 20
    business_unit_id: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Every job is associated with a business unit. Pass the business unit"
                " id as the parameter to get the jobs."
            ),
        ),
    ] = None
    country: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Pull the jobs based on the country. Use the countries endpoint to get"
                " the list of countries."
            ),
        ),
    ] = None
    state: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Pass the state id as the parameter to get the jobs. Use the states"
                " endpoint to get the states list."
            ),
        ),
    ] = None
    job_status: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Use the job status endpoint from the master data to get the job"
                " statuses. Pass the id here to pull the matching jobs."
            ),
        ),
    ] = None
    post_on_careerportal: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Send 1 to get all the jobs that are posted on the careers page. 0 for"
                " the jobs that are not posted."
            ),
        ),
    ] = None
    fromdate: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "To get the jobs in between the dates, use this parameter (date format:"
                " mm-dd-yyyy)"
            ),
        ),
    ] = None
    todate: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "This parameter works along with the fromdate. Gives the jobs that are"
                " created between the from date and to date (date format: mm-dd-yyyy)"
            ),
        ),
    ] = None
    filter: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "When the from date and to dates are used, this parameter is mandatory."
                " Use ‘created’ as the filter value to get the jobs that are created"
                " between the from and to dates."
            ),
        ),
    ] = None
    posted_ago_days: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Pass any numeric value to get the jobs that are created within this"
                " number of days."
            ),
        ),
    ] = None
    sortorder: Annotated[
        t.Optional[str],
        Meta(
            description="Use either asc or desc to sort the job postings list.",
        ),
    ] = None
    sortby: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "This filter is used along with the sortorder. Job postings can be"
                " sorted based on the job code, created date, modified date."
            ),
        ),
    ] = None
    assigned_recruiter: Annotated[
        t.Optional[dict],
        Meta(
            description="Pass the assigned recruiter as a dictionary.",
        ),
    ] = None


class ReadApplicantsParameters(Struct):
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="Number of items to pull from CEIPAL",
        ),
    ] = 20
    created_by: Annotated[
        t.Optional[int],
        Meta(
            description="User ID of the applicant creator",
        ),
    ] = None
    source: Annotated[
        t.Optional[int],
        Meta(
            description="Source ID of the applicants",
        ),
    ] = None
    applicant_status: Annotated[
        t.Optional[int],
        Meta(
            description="Applicant status ID",
        ),
    ] = None
    sortorder: Annotated[
        t.Optional[str],
        Meta(
            description="Sort order (asc or desc)",
        ),
    ] = None
    sortby: Annotated[
        t.Optional[str],
        Meta(
            description="Sort by field (e.g., applicant_id)",
        ),
    ] = None


def get_tokens(parameters: AuthParameters):
    url = "{}/createAuthtoken".format(CEIPAL_API_URL)
    payload = {
        "username": parameters.email,
        "password": parameters.password,
        "api_key": parameters.api_key,
    }

    response = requests.post(url, json=payload)
    if response.status_code // 100 != 2:
        raise Exception(
            f"Error in fetching token: {response.text}, response status"
            f" code{response.status_code}"
        )

    access_token = response.json()["access_token"]
    refresh_token = response.json()["refresh_token"]

    return access_token, refresh_token


def refresh_access_token(refresh_token: str):
    url = "{}/refreshToken".format(CEIPAL_API_URL)

    headers = {"Content-Type": "application/json", "Token": "Bearer {access_token}"}
    params = {"Token": refresh_token}
    response = requests.post(url, headers=headers, params=params)

    if response.status_code // 100 != 2:
        raise Exception(
            f"Error in refreshing token: {response.text}, response status"
            f" code{response.status_code}"
        )

    return response.json()["access_token"]


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobPostingsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    jobs_list_url = "{}/getJobPostingsList".format(CEIPAL_API_URL)
    access_token, refresh_token = get_tokens(auth_parameters)

    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {access_token}",
    }
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    limit = params.pop("limit")
    params["limit"] = limit if limit <= CEIPAL_ENDPOINT_LIMIT else CEIPAL_ENDPOINT_LIMIT

    jobs = []

    # TODO: verify that pagination is handled correctly
    while True:
        response = requests.get(jobs_list_url, headers=headers, params=params)

        if response.status_code // 100 == 2:
            jobs.extend(response.json())
            if len(jobs) == 0 or len(jobs) >= limit:
                break
            params["offset"] += CEIPAL_ENDPOINT_LIMIT

        else:
            if "access_token expired" in response.json()["detail"]:
                access_token = refresh_access_token(refresh_token)
                headers["Authorization"] = f"Bearer {access_token}"
            else:
                raise Exception(
                    f"Error in fetching jobs: {response.text} with status code:"
                    f" {response.status_code}"
                )

    if len(jobs) > limit:
        jobs = jobs[:limit]

    for job in jobs:
        full_job_response = requests.get(
            "{}/getJobPostingDetails".format(CEIPAL_API_URL),
            headers=headers,
            params={"job_id": job["id"]},
        )
        if full_job_response.status_code // 100 != 2:
            raise Exception(
                f"Error in fetching job details: {full_job_response.text} with"
                f" status code: {full_job_response.status_code}"
            )
        yield full_job_response.json()


def read_applicants(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadApplicantsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    applicants_list_url = "{}/getApplicantsList".format(CEIPAL_API_URL)
    access_token, refresh_token = get_tokens(auth_parameters)

    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {access_token}",
    }
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    limit = params.pop("limit")
    params["limit"] = limit if limit <= CEIPAL_ENDPOINT_LIMIT else CEIPAL_ENDPOINT_LIMIT

    applicants = []

    # TODO: verify that pagination is handled correctly
    while True:
        response = requests.get(applicants_list_url, headers=headers, params=params)

        if response.status_code // 100 == 2:
            applicants.extend(response.json())
            if len(applicants) == 0 or len(applicants) >= limit:
                break
            params["offset"] += parameters.limit

        else:
            if "access_token expired" in response.json()["detail"]:
                access_token = refresh_access_token(refresh_token)
                headers["Authorization"] = f"Bearer {access_token}"
            else:
                raise Exception(
                    f"Error in fetching applicants: {response.text} with status code:"
                    f" {response.status_code}"
                )

    if len(applicants) > limit:
        applicants = applicants[:limit]

    for applicant in applicants:
        if applicant["resume_path"]:
            resume_response = requests.get(applicant["resume_path"], headers=headers)
            if resume_response.status_code // 100 != 2:
                raise Exception(
                    f"Error in fetching resume: {resume_response.text} with"
                    f" status code: {resume_response.status_code}"
                )
            applicant["resume"] = resume_response.content.decode("utf-8")

        yield applicant


JobsAisle = Aisle(
    name=Entity.job,
    schema=JobPostingDetails,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobPostingsParameters,
            update=ReadJobPostingsParameters,
            archive=ReadJobPostingsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=Applicant,
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
)
