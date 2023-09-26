import requests
import typing as t
from logging import LoggerAdapter
from pydantic import Field, HttpUrl
from hrflow_connectors.connectors.digitalrecruiters.schema import (
    DigitalRecruitersJob,
    DigitalRecruitersReadProfile,
    DigitalRecruitersWriteProfile,
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

DIGITAL_RECRUITERS_JOBS_ENDPOINT = "{url_environnement}/export/job-ads/{token}"
DIGITAL_RECRUITERS_WRITE_PROFILES_ENDPOINT = (
    "{url_environnement}/api/candidate/apply/{token}"
)
DIGITAL_RECRUITERS_READ_PROFILES_ENDPOINT = "{url_environnement}/public/v1/{endpoint}"


class ReadJobsParameters(ParametersModel):
    digital_recruiters_token: str = Field(
        ...,
        description="Digital Recruiters API token.",
        repr=False,
        field_type=FieldType.Auth,
    )
    digital_recruiters_environment_url: str = Field(
        ...,
        description="Digital Recruiters API url environnement.",
        repr=False,
        field_type=FieldType.Other,
    )


class ReadProfileParameters(ParametersModel):
    api_key: str = Field(
        ...,
        description="DigitalRecruiters API key",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Username for authentication",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Password for authentication",
        repr=False,
        field_type=FieldType.Auth,
    )
    url_environment: HttpUrl = Field(
        ...,
        description="URL environment for the API",
        repr=False,
        field_type=FieldType.Other,
    )
    jobAd: int = Field(
        None,
        description="Optional: Id of a job advertisement",
        repr=False,
        field_type=FieldType.Other,
    )
    sort: str = Field(
        None,
        description=(
            "Optional: Field to sort by (id, firstName, lastName, createdAt, updatedAt)"
        ),
        repr=False,
        field_type=FieldType.Other,
    )
    limit: int = Field(
        50,
        description="Optional: Limit the number of results returned",
        repr=False,
        field_type=FieldType.Other,
    )
    page: int = Field(
        1,
        description="Optional: Page number of results returned",
        repr=False,
        field_type=FieldType.Other,
    )


class WriteProfilesParameters(ParametersModel):
    digital_recruiters_token: str = Field(
        ...,
        description="Digital Recruiters API token.",
        repr=False,
        field_type=FieldType.Auth,
    )
    digital_recruiters_environment_url: str = Field(
        ...,
        description="Digital Recruiters API url environnement.",
        repr=False,
        field_type=FieldType.Other,
    )
    job_reference: str = Field(
        ...,
        description="reference of the job to which the candidate is applying.",
        repr=False,
        field_type=FieldType.Other,
    )
    message: t.Optional[str] = Field(
        "message du candidat",
        description="Application message.",
        repr=False,
        field_type=FieldType.Other,
    )


def get_initial_token(params: ReadProfileParameters):
    url = f"{params.url_environment}/public/v1/users/login"
    headers = {"Content-Type": "application/json", "X-DR-API-KEY": params.api_key}
    payload = {"username": params.username, "password": params.password}

    response = requests.post(url, headers=headers, json=payload)

    if response.status_code == 200:
        return response.json()["token"], response.json()["refresh_token"]
    else:
        return None, None


def refresh_token(params: ReadProfileParameters, refresh_token: str):
    url = f"{params.url_environment}/public/v1/users/refresh"

    headers = {"Content-Type": "application/json", "X-DR-API-KEY": params.api_key}

    payload = {"token": refresh_token}

    response = requests.post(url, headers=headers, json=payload)

    if response.status_code == 200:
        return response.json()["token"], response.json()["refresh_token"]
    else:
        return None


def get_candidates(params: ReadProfileParameters, token):
    url = f"{params.url_environment}/job-applications/detailed"

    headers = {
        "Content-Type": "application/json",
        "X-DR-API-KEY": params.api_key,
        "Authorization": f"Bearer {token}",
    }

    response = requests.get(url, headers=headers, params=params.dict())

    if response.status_code == 200:
        data = response.json()["data"]
        return data.get("items", []), data.get("links", {}).get("next")
    else:
        return [], None


def fetch_and_add_resume(candidate, token):
    cv_url = candidate.get("CV", {}).get("url")
    if cv_url:
        headers = {"Authorization": f"Bearer {token}"}
        response = requests.get(cv_url, headers=headers)

        if response.status_code == 200:
            resume_info = {
                "raw": response.content,
                "content_type": response.headers.get("Content-Type"),
            }
            candidate["resume"] = resume_info
    return candidate


def remove_trailing_slash(url: str) -> str:
    """
    Check if the given URL ends with a slash. If yes,
    remove the slash and return the modified URL.
    If not, return the original URL.

    Args:
        url (str): The URL to check.

    Returns:
        str: The modified URL without the trailing slash
        if it had one, otherwise the original URL.
    """
    if url.endswith("/"):
        return url[:-1]
    return url


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    DR_JOB_URL = remove_trailing_slash(parameters.digital_recruiters_environment_url)
    url = DIGITAL_RECRUITERS_JOBS_ENDPOINT.format(
        url_environnement=DR_JOB_URL, token=parameters.digital_recruiters_token
    )
    adapter.info("Pulling jobs from Digital Recruiters")
    response = requests.get(url)
    if response.status_code != 200:
        adapter.error(
            "Failed to pull jobs from Digital Recruiters. Status code:"
            f" {response.status_code}."
        )
        return
    jobs = response.json().get("ads", [])
    adapter.info(f"Found {len(jobs)} jobs in Digital Recruiters.")
    if not jobs:
        adapter.warning("No jobs found in Digital Recruiters.")
        return
    for job in jobs:
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    params: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token, refresh_token = get_initial_token(params)

    if token:
        next_page_link = True  # Assume there's at least one page to fetch
        while next_page_link:
            try:
                candidates, next_page_link = get_candidates(params, token)

                if not candidates:
                    break

                for candidate in candidates:
                    candidate = fetch_and_add_resume(candidate, token)
                    yield candidate
            except Exception as e:
                adapter.info("Token expired. error: %s", e)
                refreshed_token, refresh_token = refresh_token(params, refresh_token)
                if refreshed_token:
                    token = refreshed_token
                else:
                    adapter.error("Failed to refresh token. Stopping.")
                    break


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    DR_PROFILES_URL = remove_trailing_slash(
        parameters.digital_recruiters_environment_url
    )
    url = DIGITAL_RECRUITERS_WRITE_PROFILES_ENDPOINT.format(
        url_environnement=DR_PROFILES_URL, token=parameters.digital_recruiters_token
    )

    failed_profiles = []

    for profile in profiles:
        json_data = profile
        json_data["reference"] = parameters.job_reference
        json_data["ApplicationMessage"] = dict(
            message=parameters.message  # Candidate Message
        )
        try:
            response = requests.post(url, json=json_data)
            response.raise_for_status()

            if response.status_code == 201:
                adapter.info("Candidate profile pushed successfully.")
            elif response.status_code == 202:
                adapter.warning(
                    "Candidate profile pushed, but some information is missing."
                )
            else:
                adapter.error(
                    "Failed to push candidate profile. Status code: %s, Response: %s",
                    response.status_code,
                    response.text,
                )
                failed_profiles.append(profile)

        except requests.exceptions.RequestException as e:
            adapter.exception("Error occurred while pushing candidate profile. %s", e)
            failed_profiles.append(profile)

    return failed_profiles


# Define the Warehouse for Digital Recruiters jobs and profiles
DigitalRecruitersJobWarehouse = Warehouse(
    name="DigitalRecruiters Jobs",
    data_schema=DigitalRecruitersJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[
            ActionEndpoints(
                name="Read Jobs",
                description="Read jobs from Digital Recruiters",
                url=DIGITAL_RECRUITERS_JOBS_ENDPOINT,
            )
        ],
    ),
)

# Define the Warehouse for Digital Recruiters jobs and profiles
DigitalRecruitersReadProfilesWarehouse = Warehouse(
    name="DigitalRecruiters Read Profils",
    data_schema=DigitalRecruitersReadProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfileParameters,
        function=read_profiles,
        endpoints=[
            ActionEndpoints(
                name="Read Profiles",
                description="Read profiles from Digital Recruiters",
                url=DIGITAL_RECRUITERS_READ_PROFILES_ENDPOINT,
            )
        ],
    ),
)
DigitalRecruitersWriteProfileWarehouse = Warehouse(
    name="DigitalRecruiters Write Profile",
    data_schema=DigitalRecruitersWriteProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[
            ActionEndpoints(
                name="Write Profile",
                description="Write profile to Digital Recruiters",
                url=DIGITAL_RECRUITERS_WRITE_PROFILES_ENDPOINT,
            )
        ],
    ),
)
